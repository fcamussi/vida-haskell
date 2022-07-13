module Vida where


data Celula = V | M deriving (Eq,Show)
type Tablero = [[Celula]]


{- Devuelve el tablero en forma de string lista para mostrar con putStrLn -}
mostrarTablero :: Tablero -> String
mostrarTablero [] = ""
mostrarTablero (x:xs) = filter f ((show x ++ "\n") ++ mostrarTablero xs)
                        where f x = not (elem x ['[',',',']'])


{- Cambia el estado de la célula (x,y) en el tablero -}
setCelula :: Tablero -> (Int, Int) -> Celula -> Tablero
setCelula a (x,y) c = (take (x-1) a) ++
                      [(take (y-1) b) ++ [c] ++ (drop y b)] ++
                      (drop x a)
                      where b = (a!!(x-1))


{- Obtiene el estado de la célula (x,y) en el tablero -}
getCelula :: Tablero -> (Int, Int) -> Celula
getCelula a (x,y) = (a!!(x-1))!!(y-1)


{- Obtiene la cantidad de filas del tablero -}
filas :: Tablero -> Int
filas = length


{- Obtiene la cantidad de columnas del tablero -}
columnas :: Tablero -> Int
columnas = (length.head)


{- Me dice cuantas células están vivas alrededor de la célula (x,y) -}
vecinas :: Tablero -> (Int,Int) -> Int
vecinas tablero (x,y) = length (filter (==V) v)
  where m = columnas tablero
        n = filas tablero
        v = [if y == 1 then M else getCelula tablero (x,y-1),
             if y == m then M else getCelula tablero (x,y+1),
             if x == n || y == 1 then M else getCelula tablero (x+1,y-1),
             if x == n then M else getCelula tablero (x+1,y),
             if x == n || y == m then M else getCelula tablero (x+1,y+1),
             if x == 1 || y == 1 then M else getCelula tablero (x-1,y-1),
             if x == 1 then M else getCelula tablero (x-1,y),
             if x == 1 || y == m then M else getCelula tablero (x-1,y+1)]


{- Mónada de estado como lo que vimos en clases -}
data Estado s a = Estado {runEstado :: s -> (a,s)}

instance Monad (Estado s) where
  return x = Estado (\s -> (x,s))
  (Estado h) >>= f = Estado (\s -> let (x,s') = h s
                                   in runEstado (f x) s')

{- Con la mónada Estado voy a almacenar un par de la forma (Pos,Tablero)
   Con estas funciones obtengo y actualizo la primera y segunda componentes
   de forma sencilla -}
getPosicion = Estado (\s -> (fst s,s))
getTablero = Estado (\s -> (snd s,s))
setPosicion posicion = Estado (\s -> ((),(posicion,snd s)))
setTablero tablero = Estado (\s -> ((),(fst s,tablero)))


{- Esta es la función que determina el comportamiento de la aplicación
   Acá se establecen las reglas de evolución.
   - Si una célula viva tiene alrededor de ella 2 o 3 células vivas, entonces
     sigue viva. Sino muere por soledad o por asfixia
   - Si una célula muerta tiene alrededor de ella 3 células vivas, entonces
     revive en el próximo tablero -}
transicion :: Tablero -> Estado ((Int, Int), Tablero) Tablero
transicion tablero = do
  (x,y) <- getPosicion
  tableroNuevo <- getTablero
  if y >= (columnas tablero)
    then setPosicion (x+1,1)
    else setPosicion (x,y+1)
  if x > (filas tablero)
    then return tableroNuevo
    else do
      if getCelula tablero (x,y) == V
        then if vecinas tablero (x,y) == 2 ||
                vecinas tablero (x,y) == 3
               then setTablero (setCelula tableroNuevo (x,y) V)
               else setTablero (setCelula tableroNuevo (x,y) M)
        else if vecinas tablero (x,y) == 3
               then setTablero (setCelula tableroNuevo (x,y) V)
               else setTablero (setCelula tableroNuevo (x,y) M)
      transicion tablero


{- Ejecuta la tránsicion -}
execTransicion :: Tablero -> Tablero
execTransicion tablero = fst $ (runEstado (transicion tablero)) ((1,1),tablero)

