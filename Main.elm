module Main  where 

import Fields
import Decoder
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)   
import Signal exposing (Address)
import String exposing(toLower)

type Action =
    RemoveType String
    | AddType 
    | ModifyType String Fields.Action
    | NoOp

type alias Model = Decoder.DataModel

newDataType : Decoder.DataType
newDataType =
    {
        name = "new type",
        fields = Fields.init
        
    }

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        AddType ->
                { model | dataTypes = model.dataTypes ++ [newDataType] }   

        RemoveType name ->
            { model | dataTypes = List.filter (\datatype -> datatype.name /= name) model.dataTypes }

        ModifyType name action ->            
            let updateDataType datatype =
                if datatype.name == name then
                    { datatype | fields = Fields.update action datatype.fields }
                else
                    datatype
            in 
            { model | dataTypes = List.map updateDataType model.dataTypes }


view : Address Action -> Model -> Html
view address model = 
    div[]
    [
          setupForm address model.dataTypes
        , br[][]
        --, button [ onClick address AddType ] [ text "New type" ]
    ]


-- IMPLEMENT LAZY

setupForm : Address Action -> List Decoder.DataType -> Html
setupForm address datatypes =
    div[] (List.map (checkType address) datatypes)

checkType : Address Action -> Decoder.DataType -> Html
checkType address datatype =
    if datatype.name == "Server" then --<------- change to model.root ? signal problem
        div[] 
        [ 
              h1[][ text datatype.name ] 
                -- Call the view function in fields, forward it the Address Action and the list of fields (which is the model for fields)
                -- the fields module has a model : List Field, so this works out nicely.
            , (Fields.view (Signal.forwardTo address (ModifyType datatype.name)) datatype.fields)
            , br[][]
        ]
    else
        div[][]

showField : Decoder.Field -> Html
showField field =
    div[][text field.name]

--getType : String -> Decoder.DataType
--getType kind =
--    List.filter (\datatype -> datatype.kind == kind) model.dataTypes

model : Signal Model
model =
    Signal.foldp update Decoder.getModel actions.signal

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


main : Signal Html
main =
    Signal.map (view actions.address) model