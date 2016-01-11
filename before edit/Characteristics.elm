module Characteristics (Model, init, Action, Data, update, view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.Lazy exposing(lazy2)
import Signal exposing(Address)

type alias Model = 
    {
        numOfInputs : Int,
        data : List Data    
    }

type alias Data = 
    {
        ip : String,
        id : Int
    }

type Action = NoOp | AddInput | RemoveInput Int | UpdateFieldValue Int String

init : Model
init = emptyModel

emptyModel : Model
emptyModel = 
    {
        numOfInputs = 0,
        data = []
    }

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model

    AddInput ->
    { 
        model | numOfInputs = model.numOfInputs + 1,
                data = model.data ++ [newData model.numOfInputs]
    }

    UpdateFieldValue id input ->
        let updateField data =
            if data.id == id then 
                { data | ip = input} 
            else 
                data
        in
            { model | data = List.map updateField model.data }

    RemoveInput id ->        
      {model | data = List.filter  (\d -> d.id /= id) model.data }

newData : Int -> Data
newData numOfInputs =
    {
        ip = "",
        id = numOfInputs
    }

view : Address Action -> Model -> Html
view address model =
    div[]
    [
        section[id "server-config"]
        [
            lazy2 showInputs address model.data,
            button[ onClick address AddInput ][ text "Add IP" ]
        ]
    ]

showInputs : Address Action -> List Data -> Html
showInputs address data = 
    div[] <| List.map (addInput address) data 

addInput : Address Action -> Data -> Html
addInput address data =
    div[][ 
        input 
        [
            value data.ip,
            autofocus True,
            placeholder "Enter ip here...",
            name "Server input field",
            on "input" targetValue ( Signal.message address << UpdateFieldValue data.id )
        ][],
        button [ onClick address <| RemoveInput data.id] [ text "Remove" ]
    ]

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
