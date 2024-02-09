import Data.Char (toUpper)

-- Ejercicio 1

-- Función para aplicar un descuento a un precio
-- Entrada: aplicarDescuento 100 50
-- Salida: 50.0
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento precio descuento = precio - (precio * descuento / 100)

-- Función para aplicar el IVA a un precio
-- Entrada: aplicarIVA 100 16
-- Salida: 116.0
aplicarIVA :: Double -> Double -> Double
aplicarIVA precio iva = precio + (precio * (iva / 100))

-- Función que recibe un diccionario de precios y porcentajes y una función para aplicar descuentos o IVA
-- Entrada: aplicarFuncionCesta [(50, 50), (100, 25)] aplicarDescuento
-- Salida: 100.0
aplicarFuncionCesta :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
aplicarFuncionCesta cesta funcion = sum [funcion precio descuento | (precio, descuento) <- cesta] -- Aplica una función a cada parte del diccionario y devuelve la suma de los resultados.

-- Ejercicio 2: Función que aplica una función a cada elemento de una lista
-- Entrada: aplicaFuncionLista cuadrado [1, 2, 3, 4, 5]
-- Salida: [1, 4, 9, 16, 25]
aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ [] = [] -- Caso base: lista vacía
aplicaFuncionLista f (x : xs) = f x : aplicaFuncionLista f xs -- Aplica la función al primer elemento y recursivamente al resto

-- Función para calcular el cuadrado de un número
-- Entrada: cuadrado 2
-- Salida: 4
cuadrado :: (Num a) => a -> a
cuadrado n = n * n

-- Ejercicio 3: Función que calcula la longitud de cada palabra en una frase
-- Entrada: longitudFrase "Hola mundo"
-- Salida: [("Hola", 4), ("mundo", 5)]
longitudFrase :: String -> [(String, Int)]
longitudFrase frase = [(palabra, length palabra) | palabra <- palabras] -- Crea una lista de tuplas (palabra, longitud) para cada palabra en la frase
  where
    palabras = words frase -- Divide la frase en palabras

-- Ejercicio 4: Función que convierte notas a calificaciones según ciertos criterios
-- Entrada: calificaciones [("Matemáticas", 80), ("Física", 90)]
-- Salida: [("MATEMÁTICAS", "Notable"), ("FÍSICA", "Excelente")]
calificaciones :: [(String, Int)] -> [(String, String)]
calificaciones rendimiento = [(map toUpper asignatura, convertirCalificacion nota) | (asignatura, nota) <- rendimiento]
  where
    convertirCalificacion :: Int -> String
    convertirCalificacion nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- Ejercicio 5: Función que calcula el módulo de un vector
-- Entrada: moduloVector [3, 4]
-- Salida: 5.0
moduloVector :: [Float] -> Float
moduloVector vector = sqrt (sum [x ^ 2 | x <- vector]) -- Aplica la fórmula del módulo vectorial

-- Ejercicio 6: Función que identifica valores atípicos en una muestra de números
-- Entrada: valoresAtipicos [1, 2, 3, 4, 5, 6, 9, 7, 154, 4, 5, 6, 1, 2, 3, 6, 6]
-- Salida: [154]
valoresAtipicos :: [Double] -> [Double]
valoresAtipicos muestra =
  let mediaMuestra = sum muestra / fromIntegral (length muestra) -- Calcula la media de la muestra
      desviacion = sqrt (sum [(x - mediaMuestra) ^ 2 | x <- muestra] / fromIntegral (length muestra)) -- Calcula la desviación estándar de la muestra
   in [x | x <- muestra, abs ((x - mediaMuestra) / desviacion) > 3] -- Filtra los valores atípicos basados en la puntuación típica
