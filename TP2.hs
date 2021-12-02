type Posicion = [Int]
type Jugada = (Int, Int)

zipi :: [a] -> [b] -> [(a,b)]
zipi [] ys = []
zipi xs [] = []
zipi (x:xs) (y:ys) = (x, y) : zipi xs ys

--[EJERCICIO 1]
jugar :: Posicion -> Jugada -> Posicion
jugar pos jug = jugarAux pos jug 1

jugarAux :: Posicion -> Jugada -> Int -> Posicion
jugarAux [] (t,ts) i = []
jugarAux (x:xs) (t,ts) i
    | i == t = jugarAux2(x-ts) ++ jugarAux xs (t,ts) (i+1)
    | i /= t = [x] ++ jugarAux xs (t,ts) (i+1)

jugarAux2 :: Int -> [Int]
jugarAux2 p | p == 0 = []
            | otherwise = [p]

--[EJERCICIO 2]
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas xs = generarJugadas xs 1

generarJugadas :: Posicion -> Int -> [Jugada]
generarJugadas [] _ = []
generarJugadas (x:xs) i = zipi [i,i..] [1..x] ++ generarJugadas xs (i+1)

--[EJERCICIO 3]
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p | jugadaGanadora p == (0,0) || p == [] = False
                     | otherwise = True

--[EJERCICIO 4]
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora [] = (0,0)
jugadaGanadora (y:ys) = auxJugadaGanadora (y:ys) (posiblesJugadas (y:ys))

auxJugadaGanadora :: Posicion -> [Jugada] -> Jugada
auxJugadaGanadora (y:ys) [] = (0,0)
auxJugadaGanadora (y:ys) (x:xs) | esPosicionGanadora ( jugar (y:ys) (x) ) = auxJugadaGanadora (y:ys) xs
                                | otherwise = x

--[EJERCICIO 5]
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p | jugadaGanadora p == (0,0) = 0
numeroDeJugadasGanadoras (y:ys) = numeroDeJugadasGanadorasAux (y:ys) (posiblesJugadas (y:ys))

numeroDeJugadasGanadorasAux :: Posicion -> [Jugada] -> Int
numeroDeJugadasGanadorasAux (y:ys) [] = 0
numeroDeJugadasGanadorasAux (y:ys) (x:xs) | esPosicionGanadora ( jugar (y:ys) (x) )= numeroDeJugadasGanadorasAux (y:ys) xs
                                         | otherwise = 1 + numeroDeJugadasGanadorasAux (y:ys) xs

--[EJERCICIO 6]
--pasa un n decimal a binario
naBinario :: Int -> Int
naBinario n | div n 2 == 0 = mod n 2
            | div n 2 /= 0 = mod n 2 + naBinario (div n 2) * 10
--pasa binario Int a lista [Int]
--ejemplo 1010 => [1,0,1,0]
naBinarioLista :: Int -> [Int] -> [Int]
naBinarioLista 0 y = y
naBinarioLista n y = naBinarioLista (div n 10) (agregar (mod n 2) y)

largo :: [Int] -> Int
largo [] = 0
largo (x:xs) = largo xs + 1

agregar :: Int -> [Int] -> [Int]
agregar x y = x:y
 
--esto lo escribí como a siempre menor a b en longitud
normalizarListas :: [Int] -> [Int] -> [Int]
normalizarListas a b | largo a == largo b = a
                     | largo a /= largo b = normalizarListas (agregar 0 a) b

operarEntreListas :: [Int] -> [Int] -> [Int]
operarEntreListas a b | largo a < largo b = operarEntreListasAux (normalizarListas a b) b
                      | largo a > largo b = operarEntreListasAux a (normalizarListas b a)
                      | largo a == largo b = operarEntreListasAux a b

operarEntreListasAux :: [Int] -> [Int] -> [Int]
operarEntreListasAux [] [] = []
operarEntreListasAux (x:xs) (y:ys) = agregar (xorAux x y) (operarEntreListasAux xs ys)

xor :: Int -> Int -> Int
xor n m = listaInt(operarEntreListas (naBinarioLista (naBinario n) []) (naBinarioLista (naBinario m) []))

xorAux :: Int -> Int -> Int
xorAux n m | n == 1 && m == 1 || n == 0 && m == 0 = 0
           | otherwise = 1
--pasa el binario en [Int] a Int para tener el output de xor en Int luego de haber operado
listaInt :: [Int] -> Int
listaInt [] = 0
listaInt (x:xs) | x == 1 = 2^(largo(x:xs)-x) + listaInt xs
                | x == 0 = listaInt xs
--[EJERCICIO 7]
xorLista :: [Int] -> Int
xorLista [] = 0
xorLista (x:xs) = xor x (xorLista xs)

--[EJERCICIO 8]
esPosicionGanadoraViaFormulaCerrada :: Posicion -> Bool
esPosicionGanadoraViaFormulaCerrada p | xorLista p == 0 = False
                                      | xorLista p /= 0 = True
