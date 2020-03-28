module Main where

import Difusificacion
import Desdifusificacion
import Inferencia
import Funciones

import Data.List.Split
import Text.Read
import Data.Maybe

leerValor variable repeticion
    | repeticion == True = do
        putStrLn "El valor debe de ser un número real en el rango [0, 100]"
        leerValor variable False
    | otherwise = do
        putStr $ variable ++ ": "
        input <- getLine
        let valor = readMaybe input :: Maybe Double
        if isNothing valor || valor < Just 0 || valor > Just 100
            then leerValor variable True
            else return $ fromJust valor

main = do
    ---------------- LECTURA
    cont <- readFile "archivos/Variables.txt"
    let lineas = parsearArchivo $ splitArchivo cont

    putStrLn "INGRESA TUS CALIFICACIONES"

    let variables = filter (\x -> x /= "Rendimiento") $ map fst lineas
    valores <- mapM (\x -> leerValor x False) variables
    ----------------- DIFUSIFICACION
    let difusos = difusificarVariables valores variables lineas

    --------------- INFERENCIA
    valores <- readFile "archivos/Valores.txt"

    let z = map formato $ sequence $ map snd $ map (\x -> partes $ trim x) $ splitOn "---" valores
    let evaluaciones = map (\x -> (evaluarRegla difusos x, snd x)) z
    let grupos = [[(-46)..(-20)], [(-19)..0], [1..20], [21..59]]

    let valores = map (\x -> valorFinal evaluaciones x) [(-46)..59]
    let indices = map (\y -> map (\x -> x + 46) y) grupos
    let final = salida valores indices

    ------------------- DESDIFUSIFICACION
    let gruposSalida = obtenerGrupos "Rendimiento" lineas
    let resultado = centroide $ combinarGraficas $ generarGraficas gruposSalida final

    putStr "Resultado: "
    print resultado
