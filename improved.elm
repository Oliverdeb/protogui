import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple exposing(start)
import Signal exposing(Address)


-- MODEL

-- type alias Model = List Server
-- 
-- type alias Server = {
--   hostname : String
--   , slot : String
--   , interfaces : List Interface
-- }
-- 
-- type alias Interface = {
--   name : String -- eth0
--   , ips : List IP -- [10.102.0.1, 10.103.0.1]
--   , mac : String
-- }
-- 
-- type alias IP = String

type Tree =
  Empty 
  | Node Int Tree

type alias Model = Tree

-- UPDATE

type Action = AddChild | RemoveChild Int

update : Action -> Model -> Model
update action model =
  case action of
    AddChild ->
      addChild(model)

    RemoveChild drop ->
      (removeChild model drop)

addChild : Model -> Model
addChild model =
  case model of
    Empty -> Node 1 Empty
    (Node depth child) -> Node (depth+1) (addChild child)

removeChild : Model -> Int -> Model
removeChild model drop = 
  case model of
    Empty -> Empty
    (Node depth child) ->
      if depth == drop then
        child
      else
        Node depth (removeChild child drop)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    case model of
      Empty ->
        div [] [ 
        text (toString model)
        , button [ onClick address AddChild ] [ text "Add Child" ] 
        ]
      (Node depth child) ->
        div [] 
        [
          text (toString model)
          , text "I'm child!!"
          , button [ onClick address (RemoveChild depth) ] [ text "Remove Child" ] 
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
    StartApp.Simple.start{model = Empty, view = view, update = update}
