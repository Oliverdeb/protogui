
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple exposing(start)
import Signal exposing(Address)


-- MODEL

type Tree =
    Empty
    | Node Int Tree

type alias Model =  Tree

-- UPDATE

type Action = AddChild | RemoveChild Int

update : Action -> Model -> Model
update action model=
  case action of
    AddChild ->
      addChild model

    RemoveChild index ->
      removeChild model index


addChild : Model -> Model
addChild model =
    case model of
        Empty -> 
            Node 1 Empty
        Node depth child -> 
            Node (depth+1) <| addChild child


removeChild : Model -> Int ->Model
removeChild model index =
    case model of
        Empty -> 
            Empty
        Node depth child ->
            if index == depth then
                child
            else 
                Node depth <| removeChild child index


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    case model of        
        Empty ->
            div[]
            [
              button [ onClick address AddChild ] [ text "add child" ] 
            ]
 
        Node depth child -> 
            div[]
            [
              --text <| toString model,
             input [placeholder  <| "node " ++ toString depth][ text <| "node " ++ toString depth ]
            , button [ onClick address <| RemoveChild depth ] [ text <| "remove node " ++ toString depth ]
            , view address child 
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
    StartApp.Simple.start{model  = Empty, view = view, update = update}

    