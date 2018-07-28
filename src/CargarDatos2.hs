module CargarDatos where

import ProfesorRobot
import Data.Char
import Prueba

---Este programa lee una tabla de texto plano con formato fijo y organiza la información en los tipos que el robot lee (el tipo "materia"). El separador de palabras no es ningún signo. El de campos es el espacio.


--- Una mejora próxima es que de alguna forma procese los datos de los sucesos que no son TPs y los guarde para hacer la tabla. 

--- Una mejora posterior es rehacer el tipo "Materia" para que incluya una lista de aulas. 

--- Otra mejora deseable es que sea el programa el que reparte el tiempo, contando la cantidad de semanas. 


---               FALTANTES
--- Agregar las variables nulas como la salida que da pl para no tps a la librería. 
--- leerTabla depura los eventos nulos. 

leerTabla :: [Char] -> Materia
leerTabla tablaCSV = Materia nombre dA dE eventos 
  where 
       lista = lines tablaCSV
       nombre = takeWhile (/= ' ') (lista!!1)  
       dA = nombre 
       dE = nombre
       eventos = leerEventos (drop 3 lista)


leerEventos :: [[Char]] -> [Evento]
leerEventos (l:ls) 
                  | null (l:ls)  = []
                  | length (l:ls) == 1 = [p l]
                  | otherwise = p l : (leerEventos ls)
  where
       última = map toUpper . last . words
       p l = if ( última l == "SÍ" || última l == "SI")  then (leerTP l) else (Fecha 0 0, "Nada")


leerTP :: [Char] -> Evento
leerTP línea  =  (fecha, archivo) 
  where
       palabras = words línea
       día = read ( takeWhile (/= '-') $ palabras!!0 ) :: Int
       mes = read ( (tail . dropWhile (/= '-')) $ palabras!!0 ) :: Int
       fecha = Fecha día mes 
       archivo = (unwords . tail. init) palabras


pedirTabla = do 
   putStrLn "¿Cómo se llama la tabla de la Materia a Analizar?"
   dirección <- getLine
   entrada <- readFile dirección
   let materia = leerTabla entrada
   ( putStrLn . mostrarMateria ) materia


