module DataTypes ( update, view, actions, Action) where

import Fields
import Decoder
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)   
import Signal exposing (Address)
import String exposing(toLower)


type alias Model = Decoder.DataModel


--getDataType : String -> Decoder.DataType
--getDataType kind =
--    List.filter (\datatype -> datatype.kind == kind) model.dataTypes


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
        , button [ onClick address AddType ] [ text "New type" ]
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
            , (Fields.view (Signal.forwardTo address (ModifyType datatype.name)) datatype.fields)
            , br[][]
        ]
    else
        (Fields.view (Signal.forwardTo address (ModifyType datatype.name)) datatype.fields)

showField : Decoder.Field -> Html
showField field =
    div[][text field.name]
