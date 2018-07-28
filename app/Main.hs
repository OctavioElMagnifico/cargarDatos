module Main where

import ProfesorRobot 
import CargarDatos

main :: IO ()
main = do
  archivo <- readFile "Materia.csv"
  let líneas = lines archivo
  putStrLn líneas!!1
