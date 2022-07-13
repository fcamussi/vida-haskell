module Tablero where

import Vida
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC


{- Colores -}
colorAzul = Color 0 0 65535 :: Color
colorVerde = Color 0 65535 0 :: Color
colorBlanco = Color 65535 65535 65535 :: Color


{- Dibuja una célula (viva o muerta) en la posición (x,y) -}
dibujarCelula :: DrawWindow -> (Int, Int) -> Int -> Int -> Celula -> IO ()
dibujarCelula win (x,y) m n cel = do
  (ancho,alto) <- drawableGetSize win
  gc <- gcNew win -- Toma los atributos gŕaficos actuales de la ventana de dibujo
  case cel of -- Si está viva es verde, sino blanca
    V -> gcSetValues gc newGCValues { foreground = colorVerde }
    M -> gcSetValues gc newGCValues { foreground = colorBlanco }
  let anchoCelula = ancho `div` n
      altoCelula = alto `div` m
    in drawRectangle win gc True (((y-1)*anchoCelula)+1) (((x-1)*altoCelula)+1)
                                 (anchoCelula-1) (altoCelula-1)
  return ()


{- Dibuja una fila de células -}
dibujarFila :: DrawWindow -> [Celula] -> Int -> Int -> Int -> Int -> IO ()
dibujarFila win (c:cs) m n x y = do
  if c == V then dibujarCelula win (x,y) m n V
            else dibujarCelula win (x,y) m n M
  if y < n then dibujarFila win cs m n x (y+1)
           else return ()


{- Dibuja todas las filas -}
_dibujarTablero :: DrawWindow -> Tablero -> Int -> Int -> Int -> IO ()
_dibujarTablero win (c:cs) m n x = do
  dibujarFila win c m n x 1
  if x < m then _dibujarTablero win cs m n (x+1)
           else return ()


{- Dibuja todas las células del tablero -}
dibujarTablero :: DrawWindow -> Tablero -> Int -> Int -> IO ()
dibujarTablero win cs m n = _dibujarTablero win cs m n 1


{- Dibuja las lineas divisorias
   drawSegments dibuja un conjunto de segmentos, es más rapido que hacer una
   llamada por cada segmento a dibujar -}
dibujarLineas :: DrawWindow -> Int -> Int -> IO ()
dibujarLineas win m n = do
  (ancho,alto) <- drawableGetSize win
  gc <- gcNew win
  gcSetValues gc newGCValues { foreground = colorAzul }
  let anchoCelula = ancho `div` n
      altoCelula = alto `div` m
    in do drawSegments win gc [((z*anchoCelula,0),(z*anchoCelula,altoCelula*m)) | z <- [0..n]]
          drawSegments win gc [((0,z*altoCelula),(anchoCelula*n,z*altoCelula)) | z <- [0..m]]
  return ()

