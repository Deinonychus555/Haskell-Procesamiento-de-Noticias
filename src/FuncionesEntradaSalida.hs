
module FuncionesEntradaSalida where

import TipoDatos

import FuncionesTipoDatos (leerNoticia)



-- Función que te devuelve la ruta donde están alojadas las noticias.
calcularRuta ::Int -> String
calcularRuta x ="../noticias/n" ++ show x ++ ".txt"


-- Carga todas las noticias de los archivos de texto en una coleccion de noticias.
-- El primer argumento es un entero que representa el número de noticias a procesar.
-- El segundo argumento se utiliza de contador.
cargarNoticias :: Int -> Int -> ColeccionNoticias -> IO(ColeccionNoticias)
cargarNoticias numNoticias count coleccion = do
	if count > numNoticias 
		then return coleccion
		else do
			contenido<-readFile(calcularRuta(count))
			let coleccion' = leerNoticia(contenido):coleccion
			let i = succ count
			cargarNoticias numNoticias i coleccion'


mostrarNoticias :: ColeccionNoticias -> IO()
mostrarNoticias [] = do 
	putStrLn "Lo sentimos, no se han encontrado noticias."
	putStrLn ""
	return()
mostrarNoticias (x:xs) = do
	if (xs==[]) then do
		print x
		putStrLn ""
		return()
		else do	
			print x
			putStrLn ""
			putStrLn "---------------------------------"
			putStrLn ""
			mostrarNoticias xs
				
								
mostrarCabeceras :: [Cabecera] -> IO()
mostrarCabeceras [] = do 
	putStrLn "Lo sentimos, no se han encontrado noticias."
	putStrLn ""
	return()
mostrarCabeceras (x:xs) = do
	if (xs==[]) then do
		print x
		putStrLn ""
		return()
		else do	
			print x
			putStrLn ""
			mostrarCabeceras xs
				
					
mostrarTitulos :: [Titulo] -> IO()
mostrarTitulos [] = do 
	putStrLn "Lo sentimos, no se han encontrado noticias."
	putStrLn ""
	return()
mostrarTitulos (x:xs) = do
	if (xs==[]) then do
		putStrLn x
		putStrLn ""
		return()
		else do	
			putStrLn x
			putStrLn ""
			mostrarTitulos xs
					
					
mostrarFuentes :: [Fuente] -> IO()
mostrarFuentes [] = do 
	putStrLn "Lo sentimos, no se han encontrado noticias."
	putStrLn ""
	return()
mostrarFuentes (x:xs) = do
	if (xs==[]) then do
		putStrLn x
		putStrLn ""
		return()
		else do	
			putStrLn x
			mostrarFuentes xs
					

mostrarEntidadesNombradas :: [String] -> IO()
mostrarEntidadesNombradas [] = do 
	print "Lo sentimos, no se han encontrado entidades nombradas."
	putStrLn ""
	return()
mostrarEntidadesNombradas x = do
	putStrLn (unwords x)
	putStrLn ""
	return()		
		

mostrarListasEntidadesNombradas :: [[String]] -> IO()
mostrarListasEntidadesNombradas [] = do 
	putStrLn "Lo sentimos, no se han encontrado noticias."
	putStrLn ""
	return()
mostrarListasEntidadesNombradas (x:xs) = do
	if (xs==[]) then do
		mostrarEntidadesNombradas x
		putStrLn ""
		return()
		else do	
			mostrarEntidadesNombradas x
			mostrarListasEntidadesNombradas xs				
					

mostrarListasCabeceras :: [[Cabecera]] -> Int -> IO()
mostrarListasCabeceras [] _ = do
	putStrLn "Lo sentimos, no se han encontrado noticias."
	return()
mostrarListasCabeceras (x:xs) i= do
	if xs==[] then do
		putStrLn ("Grupo "++show i)
		putStrLn "---------------"
		mostrarCabeceras x
		putStrLn ""
		return()
		else do
			putStrLn ("Grupo "++show i)
			putStrLn "---------------"
			mostrarCabeceras x
			putStrLn ""
			mostrarListasCabeceras xs (succ i)	
			
			
				
