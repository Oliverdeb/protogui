module Fields (Model, init, Action, update, view) where

import Decoder exposing (Field, getDataType)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.Lazy exposing(lazy2)
import Signal exposing(Address)
import Graphics.Element exposing(show)

type alias Model = List Field

type Action = NoOp 
            | AddField String 
            | RemoveField String 
            | UpdateFieldValue String String

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

    AddField kind ->   
         model ++ [ newField kind ]

    UpdateFieldValue name val ->
        model
        {-
        let updateField field =
            if field.name == name then 
                { field | name = val }
            else 
                field
        in
            List.map updateField model
            -}

    RemoveField name ->        
        List.filter  (\field -> field.name /= name) model

newField : String -> Field
newField kind' = 
    {
        name = "New field",
        kind = kind',
        repeated = False
    }

view : Address Action -> Model -> Html
view address model =
    div[] (List.map (displayFields address) model)


displayFields : Address Action -> Field -> Html
displayFields address field =
    
    if List.member field.kind basicTypes then     -- If the field im about to display is of type string / int then display it  otherwise
        if field.repeated then                    -- get the datatype specified (Line 77 & 82) (I dont know what it looks like.)
            div[][ 
                   p[][ text <| field.name ++ ":"]
                 , button[ onClick address <| RemoveField field.name] [ text <| " Remove " ++ field.name]
            ]
            
        else            
            p[][ text <|  field.name ++ ":" ]
    else
        if field.repeated then 
            div[][ 
                   p[][ text <|  field.name ++ ":" ]                   
                 , button[ onClick address <| RemoveField field.name] [ text <| " Remove " ++ field.name]
                 -- This add doesnt really work out because it is adding more fields to the existing server fields which doesn't make sense, it needs
                 -- to add to the list of interface fields. Need to figure out a solution to this
                 , button[ onClick address <| AddField field.kind] [ text <| " Add " ++ field.name]
                 , displayTypeFields address field.kind
            ]
        else
            div[][ 
                  p[][ text <| field.name ++ ":" ]
                , displayTypeFields address field.kind
            
            ]
        

-- Converts a Maybe a into just an a. If it cant convert it (Ie there is no value) then it just crashes and gives an error.
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

displayTypeFields : Address Action -> String -> Html
displayTypeFields address kind =

            -- What I am doing here is getting a list of datatypes corresponding to what was needed in the displayFields function.
            -- For example, "Interface" is a dataype I need to display but have no idea what it looks like from within the fields module
            -- So I use the Decoder module and get a list of datatypes corresponding to Interface (only one in this case but each server
            -- could have their own list of interfaces etc) The getDataType function returns a list and i take the head of the list, a bit 
            -- of a dodgey solution but I just wanted to see if it worked. The List.head returns a "Maybe a" type so I call the fromJust function
            -- which i found on the internet which returns the value if there is one, or throws an error if its a nothing. 

            -- I then iterate through all the fields of the given datatype displaying all the fields. 

            div[] <| List.map (displayFields address) (fromJust <| List.head <| (Decoder.getDataType kind)).fields 





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
