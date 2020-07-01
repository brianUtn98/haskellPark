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

montanaRusa::Float->Float->Atraccion
montanaRusa velocidad altura persona
  | velocidad > 50 = persona {nivelEmocion = nivelEmocion persona + 0.15*velocidad + altura}
  | otherwise = persona {nivelEmocion = nivelEmocion persona * 0.95,nivelSatisfaccion = nivelSatisfaccion persona * 0.9}

caidaLibre::Float->Atraccion
caidaLibre altura persona = persona {nivelEmocion = nivelEmocion persona + 0.2*altura}

mundoMaya::Atraccion
mundoMaya persona = persona {nivelEmocion = nivelEmocion persona * 1.10,nivelCultura = nivelCultura persona * 1.20}


showDeMagia::Atraccion
showDeMagia persona 
  | nivelCultura persona > 50 = persona {nivelSatisfaccion = nivelSatisfaccion persona + 20}
  | otherwise = persona {nivelEmocion = nivelEmocion persona + 30}

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
emocionada persona = nivelEmocion persona > 60

satisfecha::Persona->Bool
satisfecha persona = nivelSatisfaccion persona >50


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
