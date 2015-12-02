import Mouse
import Html exposing(text)
import Time 
cosWave : Signal (Float)
cosWave =  Signal.map cos (Time.every 0.2)
{-Signal.map  : (a -> b) -> Signal a -> Signal b-}

update : Signal (Int, Int)
update =  Mouse.position

main =
    Signal.map text <| Signal.map toString cosWave