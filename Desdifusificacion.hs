module Desdifusificacion where

import Difusificacion
import Data.List

-- Genera f(x) para una x en la gráfica (difusifica el valor)
valorGrafica x inf sup
  | fst inf == fst sup = snd inf
  | otherwise = difusificar x inf sup

-- Genera todos los valores de la gráfica para un grupo de una variable
graficar grupo = map (\x -> valorGrafica x (valorInferior grupo x) (valorSuperior grupo x)) [0.5,1..100]

-- Corta la gráfica hasta el valor obtenido mediante la aplicación de reglas
cortarGrafica grafica corte = map (\x -> if x <= corte then x else corte) grafica

-- Genera todas las gráficas con su respectivo corte
generarGraficas rendimiento cortes = [cortarGrafica (graficar grupo) corte | (grupo, corte) <- zip rendimiento cortes]

-- Obtiene los valores de todas las gráficas en un punto definido i
valores graficas i = map (\x -> x !! i) graficas

-- Combina las gráficas en una sóla para obtener su centroide
combinarGraficas graficas = map (\x -> maximum $ valores graficas x) [0..(length (graficas !! 0) - 1)]

-- Genera el centroide de la gráfica combinada
centroide grafica = sum [x * y | (x, y) <- zip grafica [0,0.5..100]] / sum grafica

-- Desdifusifica los valores difusos obtenidos
-- Params:
-- rendimiento -> Lista de conjuntos difusos de la variable de salida
-- cortes -> Lista de valores difusos para cada grupo [0, 0.15, 0.5, 1]
desdifusificar rendimiento cortes = centroide $ combinarGraficas $ rendimiento cortes
