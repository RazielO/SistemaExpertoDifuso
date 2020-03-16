module Difusificacion where

import Data.Maybe

-- Toma un valor xi y busca un valor en 'y' usando triangulos semejantes entre
-- (x1, y1) y (x2, y2)
difusificar xi (x1, y1) (x2, y2) = (((xi - x1) * (y2 - y1)) / (x2 - x1)) + y1

-- De una etiqueta difusa obtiene el valor superior más cercano al valor buscado
valorSuperior grupo val = (filter (\x -> val <= fst x) (snd grupo)) !! 0

-- De una etiqueta difusa obtiene el valor inferior más cercano al valor buscado
valorInferior grupo val = last (filter (\x -> val >= fst x) (snd grupo))

-- Difusifica un valor dentro de una variable
-- val = 50, variable = "HI" -> [("poco",0.0),("regular",0.7826086956521738),("mucho",0.0)]
difusificarValor :: Double -> [(String, [(Double, Double)])] -> [(String, Double)]
difusificarValor val variable = map (\x -> (fst x, difusificar val (valorInferior x val) (valorSuperior x val))) variable

obtenerGrupos llave lineas = (maybeToList (lookup llave lineas)) !! 0
