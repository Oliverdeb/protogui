
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple exposing(start)
import Signal exposing(Address)


-- MODEL

type alias Model =  Html

-- UPDATE

type Action = AddChild | RemoveChild

update : Action -> Model -> Model
update action model =
  case action of
    AddChild ->
      div[][button[{- onClick address AddChild -}] [ text "Add Child"]]

    RemoveChild ->
      div[][]


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address AddChild ] [ text "Add Child" ]
    , model {-div [ countStyle ] [ model ]-}
    , button [ onClick address RemoveChild ] [ text "Remove child" ]
    ]


countStyle : Attribute
countStyle =
  style
    [{-} ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")-}
    ]
main : Signal Html
main = 
    StartApp.Simple.start{model  = div[][], view = view, update = update}

    