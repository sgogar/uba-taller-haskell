--Funciones Auxiliares
sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

esPrimo :: Integer -> Bool
esPrimo n | sumaDivisores n == (n + 1) = True
          | otherwise = False

sumadePrimos :: Integer -> Integer -> Bool
sumadePrimos n k | k == n = False
                 | esPrimo(k) && esPrimo(n-k) = True
                 | n /= k = sumadePrimos n (k+1)

--EJERCICIO 1
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n | n > 2 && mod n 2 == 0 && sumadePrimos n 1 = True
                    | otherwise = False

--ponerlo con caso base 4 para no repetir código
--EJERCICIO 2
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n | n == 4 = True
                          | satisfaceGoldbach n = verificarConjeturaHasta (n-2)
                          | otherwise = False 

--EJERCICIO 3
auxiliarDescomposicion :: Integer -> Integer -> (Integer, Integer)
auxiliarDescomposicion n aux
                | n == 0 = (n, aux)
                | esPrimo(n-1) && esPrimo(aux+1) = (n-1, aux+1)
                | otherwise = auxiliarDescomposicion (n-1) (aux+1)

descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n
                | satisfaceGoldbach n = auxiliarDescomposicion n 0
                | otherwise = (0, 0)

--EJERCICIO 4
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n | satisfaceGoldbach n = numeroDeDescomposicionesAux n 1
                           | otherwise = undefined

numeroDeDescomposicionesAux :: Integer -> Integer -> Integer 
numeroDeDescomposicionesAux n k | k == n = 0
                             | esPrimo (n-k) && esPrimo (k) = 1 + numeroDeDescomposicionesAux n (k+1)
                             | otherwise = numeroDeDescomposicionesAux n (k+1)

--EJERCICIO 5 
másDescomponible :: Integer -> Integer
másDescomponible n = masDescomponibleAux n n

masDescomponibleAux :: Integer -> Integer -> Integer
masDescomponibleAux n k | k == 4 = n
                     | numeroDeDescomposiciones n > numeroDeDescomposiciones (k-2) = masDescomponibleAux n (k-2)
                     | otherwise = masDescomponibleAux (k-2) (k-2)
