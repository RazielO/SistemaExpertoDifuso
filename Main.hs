module Main where

import Data.List.Split
import qualified Data.Set as Set
import Difusificacion

-- Convierte un string a una lista de tuplas de dobles
-- "[(100, 0), (50, 1)]" -> [(100.0, 0.0), (50.0, 1.0)]
leerValores :: [String] -> [(Double, Double)]
leerValores linea = read (linea !! 2) :: [(Double, Double)]

-- Convierte una linea dividida a una tupla lista para ser procesada
-- ["HI","poco","[(0, 1), (15, 1)]"] -> ("HI",("poco",[(0.0,1.0),(15.0,1.0)]))
parsearLinea :: [String] -> (String, (String, [(Double, Double)]))
parsearLinea linea
  | length linea == 3 = (linea !! 0, (linea !! 1, leerValores linea))
  | otherwise = ("", ("", []))

-- Divide un archivo primero por saltos de línea, y a cada línea lo divide por "->"
leerArchivo :: [Char] -> [[[Char]]]
leerArchivo contenido = map (\x -> splitOn "->" x) (splitOn "\n" contenido)

-- Convierte un archivo en una lista de tuplas
splitArchivo :: [Char] -> [(String, (String, [(Double, Double)]))]
splitArchivo contenido = map (\x -> parsearLinea x) (leerArchivo contenido)

-- Agrupa el nombre de una variable con sus valores difusos
agruparVariable lineas llave = (llave, (filter (\x -> fst x /= "") [if (fst x == llave) then snd x else ("", []) | x <- lineas]))

-- Obtiene los nombres de las distintas variables
obtenerLlaves lineas = Set.toList (Set.filter (\x -> length x /= 0) (Set.fromList (map (\x -> fst x) lineas)))

-- Convierte el archivo entero a una lista de tuplas con las variables agrupadas
parsearArchivo lineas = map (\x -> agruparVariable lineas x) (obtenerLlaves lineas)

main = do
    variables <- readFile "archivos/Variables.txt"
    let lineas = parsearArchivo $ splitArchivo variables
    print $ difusificarValor 50 (obtenerGrupos "HI" lineas)

