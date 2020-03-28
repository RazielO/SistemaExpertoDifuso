module Inferencia where

import qualified Data.Set as Set
import Text.Read
import Data.Maybe
import Data.List.Split
import qualified Data.Text as T

filtrarNumeros aux = map (\x -> if ((readMaybe x :: Maybe Double) /= Nothing) then x else "") aux

filtrarNombres aux = filter (\x -> length x /= 0) $ map (\x -> if ((readMaybe x :: Maybe Double) /= Nothing) then "" else x) aux

valorRegla numeros = sum $ map (\x -> (read x :: Double)) $ filter (\x -> length x /= 0) numeros

obtenerValores cont = map (\x -> splitOn "->" x) $ filter (\x -> length x /= 0) $ splitOn "\n" cont

generarValoresReglas valores = zip (map (\x -> filtrarNombres x) valores) (map (\x -> valorRegla $ filtrarNumeros x) valores)

generarMatriz cont = generarValoresReglas $ sequence $ obtenerValores cont

buscarValor difusos nombre = (filter (\x -> length x /= 0) $ map (\x -> (filter (\y -> fst y == nombre)) x) difusos) !! 0 !! 0

minRegla regla difusos = minimum  $ map (\x -> snd $ buscarValor difusos x) $ fst regla

partes valor = (splitOn "+\n" valor !! 0, map (\x -> splitOn "->" x) $ splitOn "\n" $ splitOn "+\n" valor !! 1)

trim str = T.unpack $ T.strip $ T.pack str

valor lista = sum $ map (\x -> read (x !! 1) :: Double) lista

formato lista = (map (\x -> x !! 0) lista, valor lista)

evaluarRegla difusos regla = minimum $ map (\x -> snd $ buscarValor difusos x) $ fst regla

valorFinal evaluaciones grupo = sum $ map (\x -> fst x) $ filter (\y -> snd y == grupo) evaluaciones

salida valores indices = map (\y -> maximum $ map (\x -> valores !! x) y) indices
