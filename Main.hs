import Ventana
import Graphics.UI.Gtk


main :: IO ()
main = do
  initGUI -- inicializa la librería
  crearVentana
  mainGUI -- ejecuta el bucle principal y atiende eventos
  return ()

