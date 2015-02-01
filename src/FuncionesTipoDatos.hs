
module FuncionesTipoDatos where

import Data.List (nub, sort, isInfixOf, (\\))

import TipoDatos

import FuncionesBasicas hiding (cargarStopWords, obtenerLineasRestantes', obtenerEntidadesNombradas', similitud')

import OrthographicMeasures (toLowerCase, similars)



-- Dado una lista de enteros te devuelve una fecha,
-- Se supone que son numeros correspondientes a una fecha
listIntToFecha :: [Int] -> Fecha
listIntToFecha ln 
			   | esFecha = Fec dia mes anyo 
			   | otherwise = Fec 0 0 0 
	where
		dia=ln!!0
		mes=ln!!1
		anyo=ln!!2
		esFecha = length ln==3 && dia > 0 && dia <= 31 &&  mes > 0 && mes <= 12 && anyo > 1600


--Recibe una cadena que contiene una fecha y se devuelve.
--Se utiliza para capturar la fecha del archivo dónde se encuentra la noticia.
stringToFecha :: String -> Fecha
stringToFecha = listIntToFecha.obtenerNumeros
	

-- Dada una noticia devuelve el número de párrafos que contiene.
cantidadParrafos :: Noticia -> Int
cantidadParrafos = length.lines.getCuerpo


-- Dada una cadena se devuelve el título (la primera linea)
leerTitulo :: String -> Titulo
leerTitulo = obtenerLinea 1


-- Dada una cadena se devuelve la fuente (la segunda linea ignorando la fecha)
leerFuente :: String -> Fuente
leerFuente = sinNumeros.obtenerLinea 2


-- Dada una cadena se devuelve la fecha (la segunda linea ignorando la fuente)
leerFecha :: String -> Fecha
leerFecha=stringToFecha.obtenerLinea 2


-- Dada una cadena se devuelve la cabecera (contenida en las dos primeras lineas)
leerCabecera :: String -> Cabecera
leerCabecera c = Cab (leerTitulo c) (leerFuente c) (leerFecha c)


-- Dada una cadena se devuelve el cuerpo de la noticia 
-- (se ignoran las dos primeras lineas)
leerCuerpo :: String -> String
leerCuerpo = obtenerLineasRestantes 3


-- Dada una cadena se convierte a tipo noticia.
leerNoticia :: String -> Noticia
leerNoticia c = Not (leerCabecera c) (leerCuerpo c)


-- Dada una coleccion de noticias se devuelve una lista 
-- con las fuentes ordenados de las noticias.
getFuentes :: ColeccionNoticias -> [Fuente]
getFuentes noticias = sort (nub [getFuente noticia | noticia<-noticias])


-- Dada una coleccion de noticias se devuelve una lista con las 
-- cabeceras ordenados de las noticias.
getCabeceras :: ColeccionNoticias -> [Cabecera]
getCabeceras noticias = sort [getCabecera noticia | noticia<-noticias]


-- Dada una coleccion de noticias se devuelve una lista 
-- con los títulos ordenados de las noticias.
getTitulos :: ColeccionNoticias -> [Titulo]
getTitulos noticias = sort [getTitulo noticia | noticia<-noticias]


-- Dada una coleccion de noticias se devuelve una lista con los títulos 
-- ordenados de las noticias procedentes de la fuente introducida, 
-- no distingue mayusculas.
getTitulosPorFuente :: ColeccionNoticias -> Fuente -> [Titulo]
getTitulosPorFuente noticias fuente = sort titulos
	where
		titulos = [getTitulo noticia | noticia<-noticias, similars 70 fuente (getFuente noticia)]


-- Dada una coleccion de noticias se devuelve una lista con los títulos ordenados 
-- de las noticias que son de la fecha introducida.
getTitulosPorFecha :: ColeccionNoticias -> Fecha -> [Titulo]
getTitulosPorFecha noticias fecha = sort titulos
	where
		titulos = [getTitulo noticia | noticia<-noticias, fecha==getFecha noticia]


-- Dada una coleccion de noticias se devuelve una lista con los títulos ordenados 
-- de las noticias que contengan igual o mayor número de párrafos que los indicados.
getTitulosPorNumeroParrafos :: ColeccionNoticias -> Int -> [Titulo]
getTitulosPorNumeroParrafos noticias n = sort titulos
	where
		titulos = [getTitulo noticia | noticia<-noticias, n<=cantidadParrafos noticia]


-- Dada una colección de noticias y una cadena se devuelven todas las noticias cuyo
-- título contenga la cadena.
getNoticias :: ColeccionNoticias -> String -> ColeccionNoticias 
getNoticias noticias c = sort noticias2
	where
		cadena = toLowerCase c
		getTitulo' = toLowerCase.getTitulo
		noticias2 = [noticia | noticia<-noticias, cadena `isInfixOf` getTitulo' noticia ]
	
	
-- Dada una coleccion de noticias se devuelve una lista con las noticias ordenadas de 
-- aquellas procedentes de la fuente introducida (no distingue mayusculas).
getNoticiasPorFuente :: ColeccionNoticias -> Fuente -> ColeccionNoticias
getNoticiasPorFuente noticias fuente= sort noticias2
	where
		noticias2 = [ noticia | noticia<-noticias, similars 70 fuente (getFuente noticia)]


-- Dada una coleccion de noticias se devuelve una lista con las noticias ordenadas que 
-- son de la fecha indicada.
getNoticiasPorFecha :: ColeccionNoticias -> Fecha -> ColeccionNoticias
getNoticiasPorFecha noticias fecha = sort noticias2
	where
		noticias2 = [noticia|noticia<-noticias,getFecha noticia==fecha] 


-- Dada una noticia se devuelve una lista con todas las entidades nombradas
-- contenidas en el título y en el cuerpo.
getEntidadesNombradas :: Noticia -> [String]
getEntidadesNombradas noticia = obtenerEntidadesNombradas (titulo ++ "-" ++cuerpo)
	where 
	    titulo = getTitulo noticia
	    cuerpo = getCuerpo noticia
	    

-- Dada una colección de noticias se devuelve una lista con las listas de
-- las entidades nombradas de cada noticia.
getEntidadesNombradasColeccion :: ColeccionNoticias -> [[String]]
getEntidadesNombradasColeccion coleccion = [nub (getEntidadesNombradas noticia)|noticia<-coleccion]


-- El segundo argumento es una lista auxiliar donde se van metiendo las colecciones 
-- de noticias similares.
getColeccionesNoticiasSimilares' :: ColeccionNoticias -> [ColeccionNoticias]->[ColeccionNoticias]
getColeccionesNoticiasSimilares' [] colecciones = colecciones
getColeccionesNoticiasSimilares' coleccion colecciones = getColeccionesNoticiasSimilares' resto colecciones'
	where	
		cabeza = head coleccion
		cola = tail coleccion
		entidadesNombradas = getEntidadesNombradas cabeza
		noticiasSimilares = cabeza:[x|x<-cola, sonSimilares 38 entidadesNombradas (getEntidadesNombradas x)]
		resto = coleccion \\ noticiasSimilares
		colecciones' = sort noticiasSimilares:colecciones


-- Dada una coleccion de noticias se agrupan en diferentes colecciones según su similitud.		
getColeccionesNoticiasSimilares :: ColeccionNoticias -> [ColeccionNoticias]
getColeccionesNoticiasSimilares colecciones = getColeccionesNoticiasSimilares' colecciones []

