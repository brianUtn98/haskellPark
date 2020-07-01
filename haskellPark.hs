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
