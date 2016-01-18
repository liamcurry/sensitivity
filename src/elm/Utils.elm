module Utils where


import String
import Html exposing (Attribute)
import Html.Events exposing (on, targetValue)


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))


parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
    Ok value ->
      value
    Err error ->
      0.0


roundPlaces : Int -> Float -> Float
roundPlaces places number =
    let
        placesModifier = 10 ^ places
    in
        number
            |> (*) placesModifier
            |> round
            |> toFloat
            |> (flip (/)) placesModifier
