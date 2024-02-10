import Data.Char (toUpper)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- Ejercicio 1

-- Función que define las funciones matemáticas disponibles junto con sus nombres.
-- Entrada: No tiene.
-- Salida: Lista de tuplas donde cada tupla contiene un nombre de función (String) y una función que toma un entero y devuelve un Double.
funciones :: [(String, Int -> Double)]
funciones =
  [ ("sin", sin . fromIntegral), -- Entrada: Un entero. Salida: Un número en punto flotante (Double) que representa el seno del entero.
    ("cos", cos . fromIntegral), -- Entrada: Un entero. Salida: Un número en punto flotante (Double) que representa el coseno del entero.
    ("tan", tan . fromIntegral), -- Entrada: Un entero. Salida: Un número en punto flotante (Double) que representa la tangente del entero.
    ("exp", exp . fromIntegral), -- Entrada: Un entero. Salida: Un número en punto flotante (Double) que representa la exponencial del entero.
    ("log", log . fromIntegral) -- Entrada: Un entero. Salida: Un número en punto flotante (Double) que representa el logaritmo del entero.
  ]

-- Aplica una función matemática a todos los números enteros desde 1 hasta n
-- Entrada: Una cadena que representa el nombre de la función y un entero positivo n.
-- Salida: Lista de tuplas donde cada tupla contiene un número entero y el resultado de aplicar la función correspondiente a ese número entero.
aplicarFuncion :: String -> Int -> [(Int, Double)]
aplicarFuncion f n =
  [ ( i, -- Entrada: Un entero.
      case lookup f funciones of -- Busca la función correspondiente al nombre f.
        Just func -> func i -- Aplica la función al número entero i.
        Nothing -> error "Función no válida" -- Si no se encuentra la función, muestra un error.
    )
    | i <- [1 .. n] -- Genera la lista de números enteros desde 1 hasta n.
  ]

-- Imprime una tabla de valores y resultados
-- Entrada: Lista de tuplas de enteros y dobles.
-- Salida: No tiene. Imprime la tabla en la consola.
imprimirTabla :: [(Int, Double)] -> IO ()
imprimirTabla tabla = do
  putStrLn "Tabla de valores y resultados:" -- Muestra un mensaje en la consola.
  mapM_ (\(i, j) -> printf "%d\t%.4f\n" i j) tabla -- Imprime cada fila de la tabla.

-- Función principal que interactúa con el usuario
-- Entrada: No tiene.
-- Salida: No tiene. Interactúa con el usuario para solicitar la función y el entero positivo, y luego imprime la tabla de valores y resultados.
calculadora :: IO ()
calculadora = do
  putStr "Introduce la función a aplicar (sin, cos, tan, exp, log): " -- Muestra un mensaje en la consola.
  hFlush stdout -- Limpia el buffer de salida.
  f <- getLine -- Obtiene la función ingresada por el usuario.
  putStr "Introduce un entero positivo: " -- Muestra un mensaje en la consola.
  hFlush stdout -- Limpia el buffer de salida.
  nStr <- getLine -- Obtiene el número ingresado por el usuario como cadena.
  let n = read nStr :: Int -- Convierte la cadena a un número entero.
  imprimirTabla (aplicarFuncion f n) -- Imprime la tabla de valores y resultados.



-- Ejercicio 2

-- Función que filtra una lista según una función de filtro dada.
-- Entrada: Una función de filtro (que toma un elemento de tipo 'a' y devuelve un valor booleano) y una lista de elementos de tipo 'a'.
-- Salida: Una lista de elementos de tipo 'a' que pasan la condición de filtrado.
filtrarLista :: (a -> Bool) -> [a] -> [a]
filtrarLista f = filter f

-- Función booleana que verifica si un número es par
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0



-- Ejercicio 3

-- Función que asigna una calificación correspondiente a cada número entero en la lista de calificaciones dada.
-- Entrada: Una lista de números enteros.
-- Salida: Una lista de cadenas que representan las calificaciones correspondientes.
calificacionesCorrespondientes :: [Int] -> [String]
calificacionesCorrespondientes = map clasificar
  where
    clasificar calificacion
      | calificacion >= 95 && calificacion <= 100 = "Excelente" -- Si la calificación está entre 95 y 100, se clasifica como "Excelente".
      | calificacion >= 85 && calificacion <= 94 = "Notable" -- Si la calificación está entre 85 y 94, se clasifica como "Notable".
      | calificacion >= 75 && calificacion <= 84 = "Bueno" -- Si la calificación está entre 75 y 84, se clasifica como "Bueno".
      | calificacion >= 70 && calificacion <= 74 = "Suficiente" -- Si la calificación está entre 70 y 74, se clasifica como "Suficiente".
      | otherwise = "Desempeño insuficiente" -- Para cualquier otra calificación, se clasifica como "Desempeño insuficiente".



-- Ejercicio 4

-- Crea una lista de tuplas donde cada tupla contiene el nombre de una asignatura y la calificación correspondiente, convirtiendo las calificaciones a sus categorías correspondientes.
-- Entrada: Una lista de tuplas donde cada tupla contiene una cadena que representa el nombre de una asignatura y un entero que representa la calificación.
-- Salida: Una lista de tuplas donde cada tupla contiene el nombre de una asignatura en mayúsculas y la categoría correspondiente de la calificación.
asignaturasYNotas :: [(String, Int)] -> [(String, String)]
asignaturasYNotas base = [(map toUpper materia, convertirCalifs calificacion) | (materia, calificacion) <- base]
  where
    convertirCalifs calificacion
      | calificacion >= 95 && calificacion <= 100 = "Excelente" -- Si la calificación está entre 95 y 100, se clasifica como "Excelente".
      | calificacion >= 85 && calificacion <= 94 = "Notable" -- Si la calificación está entre 85 y 94, se clasifica como "Notable".
      | calificacion >= 75 && calificacion <= 84 = "Bueno" -- Si la calificación está entre 75 y 84, se clasifica como "Bueno".
      | calificacion >= 70 && calificacion <= 74 = "Suficiente" -- Si la calificación está entre 70 y 74, se clasifica como "Suficiente".
      | otherwise = "Desempeño insuficiente" -- Para cualquier otra calificación, se clasifica como "Desempeño insuficiente".



-- Ejercicio 5

-- Define un tipo de dato "Inmueble"
data Inmueble = Inmueble
  { año :: Int,
    metros :: Int,
    habitaciones :: Int,
    garaje :: Bool,
    zona :: Char
  }
  deriving (Show)

-- Calcula el precio de un inmueble dado según ciertos criterios.
-- Entrada: Un Inmueble.
-- Salida: Un valor de punto flotante que representa el precio del inmueble.
calcularPrecio :: Inmueble -> Float
calcularPrecio inmueble =
  let precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0) -- Calcula el precio base sumando el área en metros cuadrados multiplicada por 1000, el número de habitaciones multiplicado por 5000, y 15000 si tiene garaje.
      antiguedad = fromIntegral (2024 - año inmueble) -- Calcula la antigüedad del inmueble asumiendo que el año actual es 2024.
      coeficienteZona = if zona inmueble == 'B' then 1.5 else 1.0 -- Determina el coeficiente de zona, que es 1.5 si la zona es 'B', de lo contrario, es 1.0.
      precio = precioBase * (1 - antiguedad / 100) * coeficienteZona -- Calcula el precio final multiplicando el precio base por (1 - antigüedad / 100) y por el coeficiente de zona.
   in precio

-- Busca los inmuebles dentro de un presupuesto dado.
-- Entrada: Una lista de Inmuebles y un valor de punto flotante que representa el presupuesto.
-- Salida: Una lista de Inmuebles que tienen un precio menor o igual al presupuesto.
buscarInmuebles :: [Inmueble] -> Float -> [Inmueble]
buscarInmuebles inmuebles presupuesto =
  filter (\inmueble -> calcularPrecio inmueble <= presupuesto) inmuebles -- Filtra los inmuebles cuyo precio calculado sea menor o igual al presupuesto.

-- Ejemplo de uso:
inmuebles :: [Inmueble]
inmuebles = [ Inmueble { año = 2000, metros = 100, habitaciones = 3, garaje = True, zona = 'A' }
            , Inmueble { año = 2012, metros = 60, habitaciones = 2, garaje = True, zona = 'B' }
            , Inmueble { año = 1980, metros = 120, habitaciones = 4, garaje = False, zona = 'A' }
            , Inmueble { año = 2005, metros = 75, habitaciones = 3, garaje = True, zona = 'B' }
            , Inmueble { año = 2015, metros = 90, habitaciones = 2, garaje = False, zona = 'A' }
            ]

presupuesto :: Float
presupuesto = 150000.0

inmueblesEncontrados :: [Inmueble]
inmueblesEncontrados = buscarInmuebles inmuebles presupuesto