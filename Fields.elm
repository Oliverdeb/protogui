module Fields (Model, init, Action, update, view) where

import Decoder exposing (Field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.Lazy exposing(lazy2)
import Signal exposing(Address)

type alias Model = List Field

type Action = NoOp | AddField | RemoveField String | UpdateFieldValue String String

basicTypes : List String
basicTypes =
    ["String", "Int"]

init : Model
init = []

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model

    AddField ->    
        model
         --model ++ [ newField (List.head (List.reverse model)) ]

    UpdateFieldValue name val ->
        let updateField field =
            if field.name == name then 
                { field | name = val }
            else 
                field
        in
            List.map updateField model

    RemoveField name ->        
        List.filter  (\field -> field.name /= name) model

--newField : Field -> Field
--newField field = 
--    {
--        name = field.name ++ "Xx",
--        kind = "String",
--        repeated = False
--    }

view : Address Action -> Model -> Html
view address model =
    div[] (List.map (displayFields address) model)


displayFields : Address Action -> Field -> Html
displayFields address field =
    if List.member field.kind basicTypes then
        h2[][ text field.name ]
    else
        h2[][ text field.name ]
        --List.map (displayFields address) (DataTypes.getDataType field.kind)

{-
If you need to run this module, comment out line 1 and uncomment this part.

model : Signal Model
model =
    Signal.foldp update emptyModel actions.signal

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


main : Signal Html
main = 
    Signal.map (view actions.address) model


-}
