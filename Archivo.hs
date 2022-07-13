module Archivo where

import Vida
import Parsing


{- Parsea un caracter: M o V -}
cargarCelula :: Parser Celula
cargarCelula = do char 'V'
                  return V
                +++ do char 'M'
                       return M


{- Parsea el fichero completo -}
cargarFilas :: Parser Tablero
cargarFilas = do filas <- many (do fila <- many cargarCelula
                                   char '\n'
                                   return fila)
                 return filas


{- Valida si el tablero es correcto, es decir, si es una matriz y 
   no está vacío -}
validar :: Tablero -> Bool
validar tablero = if filter (/=n) (map length tablero) == [] && tablero /= []
                    then True
                    else False
                  where n = length (head tablero)


{- Carga desde un fichero el tablero, si no parsea completamente
   o no es válido el tablero retorna Nothing -}
cargarTablero :: String -> IO (Maybe Tablero)
cargarTablero fichero = do entrada <- readFile fichero
                           let e = parse cargarFilas entrada
                             in if e == [] then return Nothing
                                           else let t = (fst.head) e
                                                    z = (snd.head) e
                                                  in if null z && validar t
                                                       then return (Just t)
                                                       else return Nothing


{- Guarda el tablero a un fichero -}
guardarTablero :: String -> Tablero -> IO ()
guardarTablero fichero tablero = do writeFile fichero (mostrarTablero tablero)
                                    return ()


