module Library where
import PdePreludat

--Punto 1
data Persona = Persona {
    nombre :: String,
    satisfaccion :: Number,
    emocion :: Number,
    cultura :: Number
} deriving Show

ana = Persona "Ana" 10 20 60
juan = Persona "Juan" 20 30 40

--Punto 2
type Atraccion = Persona -> Persona

--montañaRusa
montañaRusa :: Number -> Number -> Atraccion
montañaRusa velocidad altura persona
    | (>50) velocidad = aumentarEmocion ((+altura).(*0.15) $ velocidad) persona
    | otherwise = disminuirEmocion (emocion persona * 0.05) . disminuirSatisfaccion (satisfaccion persona * 0.1) $ persona
    | otherwise = disminuirEmocion ((*0.05).emocion $ persona) . disminuirSatisfaccion ((*0.1).satisfaccion $ persona) $ persona

aumentarEmocion cantidad persona = persona {emocion = emocion persona + cantidad}

disminuirSatisfaccion cantidad persona = persona {satisfaccion = satisfaccion persona - cantidad}
disminuirEmocion cantidad persona = persona {emocion = emocion persona - cantidad}

aumentarSatisfaccion cantidad persona = persona {satisfaccion = satisfaccion persona + cantidad}
--caidaLibre
caidaLibre :: Number -> Atraccion
caidaLibre metros persona = aumentarEmocion (metros * 0.2) persona

--mundoMaya
mundoMaya :: Atraccion
mundoMaya persona = aumentarEmocion ((*0.1).emocion $ persona) . aumentarCultura ((*0.2).cultura $ persona) $ persona

aumentarCultura cantidad persona = persona { cultura = cultura persona + cantidad}

--showDeMagia
showDeMagia :: Atraccion
showDeMagia persona
    | (>50).cultura $ persona = aumentarSatisfaccion 20 persona
    | otherwise = aumentarEmocion 30 persona

--Punto 3

--visitar
visitar :: [Atraccion] -> Persona -> Persona
visitar atracciones persona = foldl (flip($)) persona atracciones

--Punto 4
--Mi abstraccion inventada, te deja como asistir a una montaña rusa de altura 50 y velocidad 100 y a un show de magia.

todas = [montañaRusa 20 20,caidaLibre 50, mundoMaya, showDeMagia, (\persona -> (montañaRusa 100 50).showDeMagia $ persona)]
--visitar todas ana

--Punto 5
--estanFelices
estanFelices :: [Persona] -> Bool
estanFelices personas = all (estaSatisfecha).filter (estaEmocionada.mundoMaya.montañaRusa 80 10) $ personas
estaEmocionada = (>60).emocion
estaSatisfecha = (>50).satisfaccion

--Punto 6
--estaContenta
estaContenta :: [Atraccion]-> Persona -> Bool
estaContenta atracciones persona = cumpleContenta.visitar atracciones $ persona

cumpleContenta persona = (>100).(+satisfaccion persona).emocion $ persona

--Punto 7
--a No se podría usar una lista infinita en 

--b 
personasInfinitas = ana:personasInfinitas
h f xs = (head.filter f) xs
--Si se puede aplicar h a un conjunto infinito de personas, ya que por lazy evaluation solo nos dará la primera que cumpla con la condición del filtro

--h (estaContenta todas) personasInfinitas
--Persona {nombre = "Ana", satisfaccion = 10, emocion = 20, cultura = 60}