module Ventana where

import Vida
import Tablero
import Archivo
import Graphics.UI.Gtk
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)


anchoCelula = 10 :: Int
altoCelula = 10 :: Int
demora = 200 :: Int -- demora entre evolución y evolución


{- Se crea la ventana y sus componentes, se setean los eventos y se
   establecen algunas características visuales
   tableroMV y timeoutMV son variables que almacenan un valor
   en "alguna parte del sistema", se crean inicialmente vacías -}
crearVentana :: IO ()
crearVentana = do
  window <- windowNew
  darea <- drawingAreaNew -- los drawingarea contienen una ventana de dibujo
  buttonCargar <- buttonNewWithLabel "Cargar"
  buttonPausarContinuar <- buttonNewWithLabel "Pausar/Continuar"
  buttonGuardar <- buttonNewWithLabel "Guardar"
  table <- tableNew 2 3 False -- tamaños no homogeneos
  tableAttach table buttonCargar 0 1 0 1 [Shrink,Fill] [Shrink,Fill] 0 0 -- encoge y rellena
  tableAttach table buttonPausarContinuar 1 2 0 1 [Shrink,Fill] [Shrink,Fill] 0 0
  tableAttach table buttonGuardar 2 3 0 1 [Shrink,Fill] [Shrink,Fill] 0 0
  frame <- aspectFrameNew 0.5 0.5 Nothing -- usa el ratio de proporción de darea
  frameSetShadowType frame ShadowNone -- quita la sombra
  containerAdd frame darea
  tableAttach table frame 0 3 1 2 [Fill,Expand] [Fill,Expand] 10 10 -- expande y rellena
  set window [ containerBorderWidth := 5,
               containerChild := table,
               windowTitle := "El juego de la vida de Conway" ]
  tableroMV <- newEmptyMVar
  timeoutMV <- newEmptyMVar
  -- conecta los handles
  onDestroy window mainQuit -- mainQuit es un handle estandar proporsionado por la librería
  on darea exposeEvent (dibujar tableroMV)
  onClicked buttonCargar (cargar tableroMV window darea) 
  onClicked buttonPausarContinuar (pausarContinuar tableroMV timeoutMV darea)
  onClicked buttonGuardar (guardar tableroMV window) 
  widgetShowAll window


{- Maneja el evento Expose. Es decir, es llamada cada vez que hay que
   redibujar la pantalla (actualizacion del tablero, redimensionamiento
   o solapamiento de la ventana)
   liftIO: me transforma la monada IO encapsulando su valor en una
   monada EventM -}
dibujar :: MVar Tablero -> EventM EExpose Bool
dibujar tableroMV = do
  win <- eventWindow -- ventana de dibujo
  liftIO (do
    vacio <- isEmptyMVar tableroMV
    if vacio
      then return False
      else do
        tablero <- readMVar tableroMV -- readMVar hace un takeMVar y un putMVar
        let m = filas tablero
            n = columnas tablero
          in do
            dibujarTablero win tablero m n
            dibujarLineas win m n
        return True)


{- Evento para el botón cargar
   Pone en tableroMV el tablero inicial que se selecciona -}
cargar :: MVar Tablero -> Window -> DrawingArea-> IO ()
cargar tableroMV window darea = do
  f <- dialogoAbrir window
  case f of
    Nothing -> return ()
    Just fichero -> do
      t <- cargarTablero fichero
      case t of
        Just tablero -> do
          vacio <- isEmptyMVar tableroMV
          if vacio
            then putMVar tableroMV tablero
            else modifyMVar_ tableroMV (\_ -> return tablero) -- modifyMVar hace un take y con el valor obtenido hace un put
          let m = filas tablero
              n = columnas tablero
            in do widgetSetSizeRequest darea (anchoCelula*n) (altoCelula*m)
                  widgetQueueDraw darea -- coloca darea en la cola para ser dibujado
        Nothing -> dialogoMensaje "El fichero no contiene un tablero válido"
                                  window


{- Evento para el botón pausarContinuar
   Si timeoutMV está vacía entonces se crea un contador y se almacena
   el handle en timeoutMV, sino se remueve el contador -}
pausarContinuar :: MVar Tablero -> MVar HandlerId -> DrawingArea -> IO ()
pausarContinuar tableroMV timeoutMV darea = do
  vacio <- isEmptyMVar tableroMV
  if vacio
    then return ()
    else do
      vacio <- isEmptyMVar timeoutMV
      if vacio
        then do
          handle <- timeoutAdd (evolucion tableroMV darea) demora
          putMVar timeoutMV handle
        else do
          handle <- takeMVar timeoutMV
          timeoutRemove handle


{- Evento para el botón guardar -}
guardar :: MVar Tablero -> Window -> IO ()
guardar tableroMV window = do
  vacio <- isEmptyMVar tableroMV
  if vacio
    then return ()
    else do
      f <- dialogoGuardar window
      case f of
        Nothing -> return ()
        Just fichero -> do tablero <- readMVar tableroMV
                           guardarTablero fichero tablero


{- Función que se llama cada cierto perdiodo de tiempo, actualiza tableroMV
   con el tablero siguiente y pide que se redibuje -}
evolucion :: MVar Tablero -> DrawingArea -> IO Bool
evolucion tableroMV darea = do
  tablero <- readMVar tableroMV
  --putStrLn (mostrarTablero tablero) -- muestra por consola lo mismo que en la ventana
  modifyMVar_ tableroMV (\ tablero -> return (execTransicion tablero))
  widgetQueueDraw darea
  return True


{- Muestra un diálogo de selección de fichero para cargar y retorna el nombre
   del fichero si seleccionó alguno, sino retorna Nothing -}
dialogoAbrir :: Window -> IO (Maybe String)
dialogoAbrir window = do
  dialog <- fileChooserDialogNew (Just "Abrir") (Just window) 
                                 FileChooserActionOpen
                                 [("gtk-open", ResponseAccept),
                                  ("gtk-cancel", ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  case response of
    ResponseAccept -> do fichero <- fileChooserGetFilename dialog
                         return fichero
    ResponseCancel -> return Nothing
    ResponseDeleteEvent -> return Nothing


{- Igual que antes pero para guardar -}
dialogoGuardar :: Window -> IO (Maybe String)
dialogoGuardar window = do
  dialog <- fileChooserDialogNew (Just "Guardar") (Just window)
                                 FileChooserActionSave
                                 [("gtk-save", ResponseAccept),
                                  ("gtk-cancel", ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  widgetHide dialog
  case response of
    ResponseAccept -> do fichero <- fileChooserGetFilename dialog
                         return fichero
    ResponseCancel -> return Nothing
    ResponseDeleteEvent -> return Nothing


{- Muestra un mensaje de tipo warnning -}
dialogoMensaje :: String -> Window -> IO ()
dialogoMensaje mensaje window = do
  dialog <- messageDialogNew (Just window) [DialogModal]
                             MessageWarning ButtonsOk mensaje
  widgetShow dialog
  dialogRun dialog
  widgetHide dialog
  return ()

