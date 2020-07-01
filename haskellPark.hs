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
