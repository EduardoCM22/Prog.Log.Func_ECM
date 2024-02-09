-- Función lessThan20: Devuelve la representación en palabras de un número menor que 20.
-- Entrada: Un número entero n.
-- Salida: La representación en palabras de n si n es mayor que 0 y menor que 20, de lo contrario, devuelve una cadena vacía.
lessThan20 :: Int -> String
lessThan20 n
  | n > 0 && n < 20 -- Verifica si n está en el rango [1, 19].
    =
      let answers = words ("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen") -- Crea una lista de palabras del uno al diecinueve.
       in answers !! (n - 1) -- Devuelve la palabra en la posición (n - 1) de la lista answers.

-- Función teens: Devuelve la representación en palabras de un número entre 20 y 99 (múltiplos de 10).
-- Entrada: Un número entero n.
-- Salida: La representación en palabras de n si n es un múltiplo de 10 entre 20 y 90, de lo contrario, devuelve una cadena vacía.
teens :: Int -> String
teens n
  | n >= 2 && n <= 9 -- Verifica si n está en el rango [2, 9].
    =
      answers !! (n - 2) -- Devuelve la palabra en la posición (n - 2) de la lista answers.
  where
    answers = words ("twenty thirty fourty fifty sixty seventy eighty ninety") -- Crea una lista de palabras para los múltiplos de 10 entre 20 y 90.

-- Función number: Devuelve la representación en palabras de un número.
-- Entrada: Un número entero n.
-- Salida: La representación en palabras de n según ciertas reglas establecidas.
number :: Int -> String
number n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz!" -- Verifica si n es divisible por 3 y por 5.
  | n `mod` 3 == 0 = "Fizz!" -- Verifica si n es divisible por 3.
  | n `mod` 5 == 0 = "Buzz!" -- Verifica si n es divisible por 5.
  | 1 <= n && n < 20 = lessThan20 (n) -- Verifica si n está entre 1 y 19.
  | n `mod` 10 == 0 && n < 100 = teens (n `div` 10) ++ "!" -- Verifica si n es un múltiplo de 10 entre 20 y 90.
  | n < 100 = teens (n `div` 10) ++ " " ++ lessThan20 (n `mod` 10) ++ "!" -- Verifica si n está entre 20 y 99.
  | n == 100 = "One hundred!" -- Verifica si n es igual a 100.
