module Lib () where

import Text.Show.Functions ()

type Poder = Personaje -> Personaje

data Personaje = UnPersonaje {
    nombre :: String,
    poder_basico :: Poder,
    super_poder :: Poder,
    super_activo :: Bool,
    cantidad_vida :: Int
}deriving Show

--Poderes 
bolaEspinosa :: Personaje -> Personaje
bolaEspinosa unPersonaje = unPersonaje {cantidad_vida = max (cantidad_vida unPersonaje - 1000) 0}

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoLluvia unPersonaje
    |tipoLluvia == "sanadora" = unPersonaje {cantidad_vida = cantidad_vida unPersonaje + 800}
    |tipoLluvia == "dañina" = unPersonaje {cantidad_vida = div (cantidad_vida unPersonaje) 2}
    |otherwise = unPersonaje

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio unPersonaje
    |radio > 3 && cantidad_vida unPersonaje < 800 = unPersonaje {nombre = nombre unPersonaje ++ " Espina estuvo aquí",super_activo = False, cantidad_vida = 0}
    |radio > 3 = unPersonaje {nombre = nombre unPersonaje ++ " Espina estuvo aquí"}
    |otherwise = bolaEspinosa unPersonaje

torretaCurativa:: Personaje -> Personaje
torretaCurativa unPersonaje = unPersonaje {super_activo= True, cantidad_vida = cantidad_vida unPersonaje * 2}

ataqueEspecial ::  Personaje -> Personaje -> Personaje
ataqueEspecial atacante atacado
    |super_activo atacante = (poder_basico atacante).(super_poder atacante) $ atacado 
    |otherwise = atacado

pocaVida :: Personaje -> Bool
pocaVida unPersonaje = cantidad_vida unPersonaje < 800

estanEnLasUltimas :: [Personaje] -> [String]
estanEnLasUltimas listaPersonajes = (map nombre).(filter pocaVida) $ listaPersonajes 

--personajes
espina :: Personaje
espina = UnPersonaje{
    nombre= "Espina",
    poder_basico = bolaEspinosa,
    super_poder = granadaDeEspinas 5,
    super_activo = True,
    cantidad_vida = 4800
}

pamela :: Personaje
pamela = UnPersonaje{
    nombre= "Pamela",
    poder_basico = lluviaDeTuercas "sanadora",
    super_poder = torretaCurativa,
    super_activo = False,
    cantidad_vida = 9600
}
