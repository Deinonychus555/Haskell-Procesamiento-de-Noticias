
module FuncionesBasicas where

import Data.Char (digitToInt, isAlpha, isDigit, isAlphaNum, isUpper)

import Data.List (intercalate, groupBy, find, nub)

import OrthographicMeasures (toLowerCase, getThreshold,similars)

import System.IO.Unsafe


-- Función para cargar las 'stopwords' de un fichero.
cargarStopWords :: IO([String])
cargarStopWords= do
	stopWords<-readFile("../diccionarios/stopwords.txt")
	return (words stopWords)
	

-- Función que crea un alista con las 'stopwords'.	
stopWords :: [String]
stopWords = unsafePerformIO cargarStopWords


-- Convierte una cadena a entero.				
stringToInt :: String -> Int
stringToInt [] = 0
stringToInt (x:xs) = (digitToInt x) * 10 ^ (length xs ) + stringToInt xs


-- Dado un índice te devuelve dicha linea de una cadena.
obtenerLinea :: Int -> String -> String
obtenerLinea n c = (lines c)!!(n-1)


-- El segundo argumento es un contador.
obtenerLineasRestantes' :: Int -> Int -> [String] -> String
obtenerLineasRestantes' n count lc
							| count == n = intercalate "\n" lc
							| otherwise = obtenerLineasRestantes' n (succ count) (tail lc)


-- Dado un índice te devuelve todas las lineas de una cadena 
-- a partir de dicho índice incluido.
obtenerLineasRestantes :: Int -> String -> String
obtenerLineasRestantes n c = obtenerLineasRestantes' n 1 (lines c)

				
-- Comprueba si la cadena representa un número (que todos su caracteres sean dígitos)
esNumero :: String -> Bool
esNumero c 
          | c =="" = False 
          | otherwise = all (isDigit) c


-- Comprueba si la cadena representa una palabra (que todos su caracteres sean letras)
esPalabra :: String -> Bool
esPalabra c 
          | c =="" = False 
          | otherwise = all (isAlpha) c


-- Comprueba si la cadena contiene algun caráter no alfanumérico.
esAlfaNumerico :: String -> Bool
esAlfaNumerico c 
          | c =="" = False 
          | otherwise = all (isAlphaNum) c


-- Comprueba si la cadane es una 'stopword'
esStopWord :: String -> Bool
esStopWord c = toLowerCase c `elem` stopWords 


-- Comprueba si la cadena representa una entidad nombrada
esEntidadNombrada :: String -> Bool
esEntidadNombrada [] = False
esEntidadNombrada (x:xs) =  not (esStopWord (x:xs)) && isUpper x
	
					
-- Elimina de la lista aquellas cadenas que contengan algún carácter
-- no alfanumérico.          
limpiarCadenas :: [String] -> [String]
limpiarCadenas = filter (esAlfaNumerico)


-- Dada una cadena devuelve una lista de subcadenas de la original resultantes
-- de la aparición de caracteres no alfanuméricos.   
agrupar :: String -> [String]
agrupar = groupBy (\a b-> isAlphaNum b && isAlphaNum a )


-- Dada una cadena la va partiendo según encuentre caracteres 
-- no alfanuméricos y elimina estos caracteres, devolviendo
-- una lista con las subcadenas resultantes.
obtenerCadenasLimpias :: String -> [String]
obtenerCadenasLimpias c = limpiarCadenas (agrupar c)


-- Dada una cadena, aquellas subcadenas que representen números se convertirán 
-- a tipo entero y se agregaran a la lista de salida.
-- Esta función la utilizo para calcular la fecha.
obtenerNumeros :: String -> [Int]
obtenerNumeros c = [stringToInt c' | c'<-obtenerCadenasLimpias c, esNumero c']


-- Dada una cadena, se devolverá una lista con aquellas subcadenas que  
-- que representen palabras (incluidas stopwords).
obtenerPalabras :: String -> [String]
obtenerPalabras c = [c' | c'<-obtenerCadenasLimpias c, esPalabra c']


-- Recibe una cadena y devuelve otra en la que se ignoran los números.
-- Se utiliza para capturar la fuente del archivo dónde se encuentra la noticia.
-- Esta función la utilizo para obtener el nombre de la fuente de la línea de texto.
sinNumeros :: String -> String
sinNumeros  = unwords.obtenerPalabras 


obtenerEntidadesNombradas' :: [String] -> String -> [String] -> [String]
obtenerEntidadesNombradas' [] c l
								| c==[] = reverse l
								| otherwise = reverse (c:l)
obtenerEntidadesNombradas' (x:xs) c l
									| x == " " = self xs c l 
									| esEntNomX = self xs (c++x) l
									| (not esEntNomX) && (c /= []) = self xs [] (c:l)
									| otherwise = self xs c l
	where
		self = obtenerEntidadesNombradas'	
		esEntNomX = esEntidadNombrada x	
		
		
-- Dada una cadena, se devolverá una lista con aquellas cadenas que
-- sean entidades nombradas.
-- Ejemplo: "Felipe VI de España" -> ["FelipeVI","España"]
obtenerEntidadesNombradas :: String -> [String]
obtenerEntidadesNombradas c = obtenerEntidadesNombradas' (agrupar c) "" []


-- Dada una cadena, comprueba si existe en una lista una cadena similar.
-- Recibe un entero que representa el grado de similitud.
existeSimilar :: Integer -> String -> [String] -> Bool
existeSimilar = (\p c l -> (find (similars p c ) l) /= Nothing) 


-- El entero que recibe es el contador de repeticiones similares encontradas
numeroOcurrencias :: [String] -> [String] -> Int -> Int
numeroOcurrencias [] _ n = n
numeroOcurrencias (x:xs) l n = if (existeSimilar 42 x l) then 
								numeroOcurrencias xs l (succ n)
								else  numeroOcurrencias xs l n


-- Dadas dos listas de cadenas  te devuelve el porcentaje de similitud entre ellas
-- siguiendo la siguiente fórmula:
-- cadenas similares en ambas listas * 2 / suma de las longitudes de las listas
similitud' :: [String] -> [String] -> Float
similitud' l1 l2 = a/b
	where
		 a = fromIntegral (numeroOcurrencias l1 l2 0)*2
		 b = fromIntegral (length l1 + length l2)


-- Dado un umbral mínimo y dos cadenas, comprueba si la similitud de dichas
-- cadenas supera el umbral.
sonSimilares :: Integer -> [String] -> [String] -> Bool
sonSimilares p l1 l2 = (similitud' l1' l2') >= getThreshold p
	where 
		l1' = nub l1
		l2' = nub l2
