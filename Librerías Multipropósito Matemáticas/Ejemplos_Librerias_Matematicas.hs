import Numeric.LinearAlgebra -- hmatrix
import Statistics.Sample     -- statistics
import Math.Polynomial       -- polynomial
import SymPy                 -- sympy
import Math.Functions        -- math-functions
import Numeric.GSL.ODE       -- hmatrix-gsl 
import Data.VectorSpace      -- vector-space

-- hmatrix

-- Crear una matriz
matriz = (3><3) [1,2,3,
                 4,5,6,
                 7,8,9] :: Matrix Double

-- Calcular la inversa de la matriz
inversa = inv matriz
-- Multiplicar la matriz original por su inversa para obtener la identidad
identidad = matriz `multStd` inversa

-- identidad = ((1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,0.0,1.0))



-- statistics

-- Calcular la media de una lista de números
media = mean [1.0, 2.0, 3.0, 4.0, 5.0]

-- Calcular la desviación estándar de una lista de números
desviacion = stdDev [1.0, 2.0, 3.0, 4.0, 5.0]

-- media = 3.0
-- desviacion = 1.4142135623730951



-- polynomial

-- Crear un polinomio
polinomio = P [1, 0, -3, 2] -- xx^3 - 3x + 2

-- Evaluar el polinomio en un valor específico
resultadoPolinomio = eval polinomio 2 -- Evalúa el polinomio en x = 2

-- resultadoPolinomio = 4



-- sympy

-- Resolver una ecuación simbólicamente
resultadoSimPy = solve (x^2 - 4) x

-- resultadoSimPy = [-2, 2]



-- math-functions

-- Calcular la función seno de un ángulo en radianes
seno = sin 1.0

-- Calcular la exponencial de un número
exponencial = exp 2.0

-- seno = 0.8414709848078965
-- exponencial = 7.38905609893065



-- hmatrix-gsl

-- Definir una ecuación diferencial
derivada :: Double -> [Double] -> [Double]
derivada t [x, y] = [y, -x]

-- Resolver la ecuación diferencial
solucionODE = solveV ODE.AdamsBashforth 0.01 1E-6 1000 derivada [0, 1]

-- solucionODE = *ejecutar la funcion*  



-- vector-space

-- Definir vectores en 2D
vector1 = (3, 4)
vector2 = (1, 2)

-- Sumar vectores
sumaVectores = vector1 ^+^ vector2

-- Calcular el producto escalar
productoEscalar = vector1 `dot` vector2

-- sumaVectores = (4, 6)
-- productoEscalar = 11
