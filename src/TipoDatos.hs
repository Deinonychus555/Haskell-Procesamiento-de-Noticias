
module TipoDatos where



type Dia = Int
type Mes = Int
type Anyo = Int

data Fecha = Fec Dia Mes Anyo deriving Eq

-- Se utiliza para mostrar un 0 delante de los días y meses de un solo dígito.
ajustarFecha::Int->String
ajustarFecha x 
				| x <=9 = "0" ++ show x
				| otherwise = show x
							
instance Show Fecha where
	show (Fec d m a) = ajustarFecha d ++ "/" ++ ajustarFecha m ++ "/" ++ ajustarFecha a

instance Ord Fecha where
	(Fec d1 m1 a1 ) < (Fec	d2 m2 a2 ) = (d1<d2) && (m1<=m2) && (a1<=a2)
										|| (m1<m2) && (a1<=a2)
										|| (a1<a2)
	f1 <= f2 = f1<f2 || f1==f2	
	(Fec d1 m1 a1 ) > (Fec	d2 m2 a2 ) = (d1>d2) && (m1>=m2) && (a1>=a2)
										|| (m1>m2) && (a1>=a2)
										|| (a1>a2)
	f1 >= f2 = f1>f2 || f1==f2								



type Titulo=String
type Fuente=String

data Cabecera= Cab Titulo Fuente Fecha deriving Eq

instance Show Cabecera where
	show (Cab t f fe) =  t ++ "\n" ++ f ++ " - " ++ show fe 
	
-- Una cabecera se considera menor que otra si su fecha es menor, en caso
-- de que las fechas sean iguales se considera menor la de título menor.	
instance Ord Cabecera where
	(Cab t1 f1 fe1) < (Cab t2 f2 fe2) = fe1<fe2 || (fe1==fe2) && (t1<t2)
	cabecera1  <= cabecera2 = cabecera1  < cabecera2  || cabecera1  == cabecera2 
	(Cab t1 f1 fe1) > (Cab t2 f2 fe2) = fe1>fe2 || (fe1==fe2) && (t1>t2)
	cabecera1  >= cabecera2 = cabecera1  > cabecera2  || cabecera1  == cabecera2 
	
	

type Cuerpo = String
	
data Noticia= Not Cabecera Cuerpo 

instance Eq Noticia where
	(Not (Cab t1 f1 fe1) c1) == (Not (Cab t2 f2 fe2) c2) = (t1==t2) && (f1==f2) && (fe1==fe2)

instance Ord Noticia where
	(Not cab1 c1) < (Not cab2 c2) = cab1<cab2
	noticia1 <= noticia2 = noticia1<noticia2 || noticia1==noticia2
	(Not cab1 c1) > (Not cab2 c2) = cab1>cab2
	noticia1 >= noticia2 = noticia1>noticia2 || noticia1==noticia2
	
instance Show Noticia where
	show (Not cab cue) = show cab ++ "\n" ++ "\n" ++ cue 


type ColeccionNoticias = [Noticia] 

			
 							
-- Dada una noticia se devuelve la fecha de ésta.
getFecha :: Noticia -> Fecha
getFecha (Not (Cab t f fecha) c)=fecha


-- Dada una noticia se devuelve el título de ésta.
getTitulo :: Noticia -> Titulo
getTitulo (Not (Cab titulo f fe) c)=titulo


-- Dada una noticia se devuelve la fuente de ésta.
getFuente :: Noticia -> Fuente
getFuente (Not (Cab t fuente f) c)=fuente


-- Dada una noticia se devuelve la cabecera de ésta.
getCabecera :: Noticia -> Cabecera
getCabecera (Not cabecera cuerpo)=cabecera


-- Dada una noticia se devuelve el cuerpo de ésta.
getCuerpo :: Noticia -> Cuerpo
getCuerpo (Not (Cab t f fecha) cuerpo)=cuerpo




