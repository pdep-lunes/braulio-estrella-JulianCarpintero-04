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