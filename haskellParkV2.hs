import Text.Show.Functions

--Punto 1 Modelado de las personas que asisten al parque.

data Persona  = Persona {
nombre::String,
nivelEmocion::Float,
nivelSatisfaccion::Float,
nivelCultura::Float
} deriving Show


-- Para no pensar en redondeo los niveles los planteamos como float. (Nada en el enunciado lo impide).

ana::Persona
ana = Persona "Ana" 20 10 60

juan::Persona
juan = Persona "Juan" 30 20 40

--Punto 2 Modelado de las atracciones

type Atraccion = Persona->Persona --Se modela el sinónimo de tipo atracción.

montanaRusa :: Float -> Float -> Atraccion
montanaRusa altura velocidad unaPersona 
	| velocidad > 50 = modificarEmocion (*1.15).modificarEmocion(+altura) $ unaPersona
    | otherwise = modificarEmocion (*0.95).modificarSatisfaccion (*0.90) $ unaPersona

caidaLibre :: Float -> Atraccion
caidaLibre metrosCaida unaPersona = modificarEmocion (+(metrosCaida*0.2)) unaPersona

mundoMaya :: Atraccion
mundoMaya unaPersona = modificarEmocion (*1.10).modificarCultura(*1.20) $ unaPersona

showdeMagia :: Atraccion
showdeMagia unaPersona 
	| (nivelCultura unaPersona) > 50 = modificarSatisfaccion (+20) unaPersona
	| otherwise = modificarEmocion (+30) unaPersona

modificarEmocion :: (Float -> Float) -> Persona -> Persona
modificarEmocion f unaPersona = unaPersona {nivelEmocion = f.nivelEmocion $ unaPersona}

modificarCultura :: (Float -> Float)  -> Persona -> Persona
modificarCultura f unaPersona = unaPersona {nivelCultura = f.nivelCultura $ unaPersona}

modificarSatisfaccion :: (Float -> Float)  -> Persona -> Persona
modificarSatisfaccion f unaPersona = unaPersona{nivelSatisfaccion = f.nivelSatisfaccion $ unaPersona}

--Punto 3 Visitar atracciones

visitar::Persona->[Atraccion]->Persona
visitar persona atracciones = foldl(flip($)) persona atracciones

--Punto 4 ejemplo de invocación y respuesta de visitar, con una función inventada (sin funciones auxiliares)

--Para resolver la función inventada se debe utilizar una lambda.
todas::[Atraccion]
todas = [montanaRusa 60 10,caidaLibre 10,mundoMaya,showDeMagia,(\persona -> mundoMaya.showDeMagia $ persona)]

--En mi caso la función que inventé te deja como si hubieses asistido a un show de magia y luego a mundo maya.

--visitar ana todas
--Persona {nombre = "Ana", nivelEmocion = 49.610004, nivelSatisfaccion = 50.0, nivelCultura = 86.4}

--Punto 5 

--Funciones auxiliares.

emocionada::Persona->Bool
emocionada persona = (>60).nivelEmocion $ persona

satisfecha::Persona->Bool
satisfecha persona = (>50).nivelSatisfaccion $ persona


--
estanFelices::[Persona]->Bool
estanFelices personas = all (satisfecha).filter (emocionada.montanaRusa 150 50.mundoMaya) $ personas

--Punto 6

--Funciones auxiliares

contenta::Persona->Bool
contenta persona = sumaEmocionSatisfaccion persona > 200

sumaEmocionSatisfaccion::Persona->Float
sumaEmocionSatisfaccion persona = nivelSatisfaccion persona + nivelEmocion persona
--

estaContenta::Persona->[Atraccion]->Bool
estaContenta persona atracciones = contenta (visitar persona atracciones)

--Punto 7
--a)
personasInfinitas::[Persona]
personasInfinitas = ana:personasInfinitas

h f xs = (head.filter f) xs
--No se podría aplicar un conjunto infinito de atracciones en visitar,
-- ya que necesitaría recorrer toda la lista para que la persona se suba a todas y cada una de las atracciones.
--b)
--Sí se puede, ya que al utilizar evaluación diferida, solo buscará la primer persona que esté contenta. 

--Falta ejemplo.
