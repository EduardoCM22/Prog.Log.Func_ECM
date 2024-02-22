-- Ejercicio 1 : Suma de elementos en una lista

sumarLista:: [Int] -> Int
sumarLista [] = 0
sumarLista lista = sum lista

-- Ejercicio 2 : Factorial

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Ejercicio 3 : Números pares

numerosPares :: Int -> [Int]
numerosPares n = [2, 4 .. n]

-- Ejercicio 4 : Longitud de una cadena

longitudCadena :: String -> Int
longitudCadena = length

-- Ejercicio 5 : Reverso de una lista

reversoLista :: [a] -> [a]
reversoLista = reverse

-- Ejercicio 6 : Duplicar elementos

duplicarElementos :: [Int] -> [Int]
duplicarElementos = concatMap (\x -> [x, x])

-- Ejercicio 7 : Filtrar elementos pares

filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

-- Ejercicio 8 : Fibonacci

fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- Ejercicio 9 : Divisores de un número

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- Ejercicio 10 : Palíndromo

esPalindromo :: String -> Bool
esPalindromo str = str == reverse str