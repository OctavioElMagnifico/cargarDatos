module Prueba where

import ProfesorRobot
---import CargarDatos

mes xs = (tail . dropWhile (/= '-')) xs!!0

prueba = do
  archivo <- readFile "Materia.csv" 
  let líneas = lines archivo
 ---let palabras = words líneas!!3
 ---let mes = mes palabras
  putStrLn (líneas!!4)
