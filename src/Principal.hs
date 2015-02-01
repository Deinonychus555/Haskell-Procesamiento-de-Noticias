
module Principal where

import TipoDatos

import FuncionesTipoDatos hiding (getColeccionesNoticiasSimilares')

import FuncionesEntradaSalida hiding (calcularRuta)



main' :: ColeccionNoticias -> IO()
main' coleccion= do
	print "Elige la opcion a realizar:"
	print "1.- Listar fuentes."
	print "2.- Buscar titulos por fuente."
	print "3.- Buscar titulos por numero de parrafos."
	print "4.- Buscar noticias."
	print "5.- Mostrar cabecera de las noticias."
	print "6.- Mostrar entidades nombradas de una fuente."
	print "7.- Buscar noticias similares por entidades nombradas."
	print "8.- Buscar titulos por fecha."
	print "9. Salir."
	opcion<-getLine
	putStrLn ""
	case (opcion) of
		"1"->	mostrarFuentes fuentes
		"2"-> do 
			putStrLn "Por favor, introduzca la fuente a buscar."
			fuente<-getLine
			putStrLn ""
			mostrarTitulos (titulosPorFuente fuente)
		"3"-> do 
			putStrLn "Por favor, introduzca un numero de parrafos."
			numParrafos<-readLn
			putStrLn ""
			mostrarTitulos (titulosPorNumeroParrafos numParrafos)
		"4"-> do 
			putStrLn "Por favor, introduzca la cadena a buscar."
			cadena<-getLine
			putStrLn ""
			mostrarNoticias (noticiasPorCadena cadena)	
		"5"-> mostrarCabeceras cabeceras	
		"6"-> do 
			putStrLn "Por favor, introduzca el nombre de la fuente."
			fuente<-getLine	
			putStrLn ""
			mostrarListasEntidadesNombradas (coleccionEntNom fuente)
		"7"-> do
			putStrLn ""
			putStrLn "- - - Puede tardar un poco, por favor, le rogamos tenga paciencia - - -"
			putStrLn ""
			putStrLn ""
			mostrarListasCabeceras listasCabeceras 1
		"8"->	do 
			putStrLn "Por favor, introduzca una fecha en formato dd/mm/aaaa."
			fecha<-getLine	
			putStrLn ""	
			mostrarTitulos (titulosPorFecha (stringToFecha fecha))
		"9"->	return()
		otherwise->do
				   print "Lo sentimos, ha marcado una opcion no valida."
				   putStrLn ""
	putStrLn ""
	if (opcion /="9") then do			   
		putStrLn "- - - Pulse una tecla para continuar o 's' para salir - - -"			   	
		tecla<-getLine
		putStrLn ""
		if (tecla=="s") then do
			putStrLn "Hasta pronto."
			return()
			else
				main' coleccion
		else
			putStrLn "Hasta pronto."		
	where
		fuentes = getFuentes coleccion
		titulosPorFuente = getTitulosPorFuente coleccion 
		titulosPorNumeroParrafos = getTitulosPorNumeroParrafos coleccion
		noticiasPorCadena = getNoticias coleccion
		cabeceras = getCabeceras coleccion
		noticiasPorFuente = getNoticiasPorFuente coleccion
		coleccionEntNom = getEntidadesNombradasColeccion.noticiasPorFuente
		colecciones = getColeccionesNoticiasSimilares coleccion
		listasCabeceras = [[getCabecera noticia|noticia<-coleccion]|coleccion<-colecciones]
		titulosPorFecha = getTitulosPorFecha coleccion
		
main :: IO()
main = do
	putStrLn "Por favor, introduzca el numero de noticias que quieras procesar."
	putStrLn "Le recordamos que hay 17 noticias disponibles."
	n<-readLn
	putStrLn ""	
	noticias<-cargarNoticias n 1 []
	main' noticias
		