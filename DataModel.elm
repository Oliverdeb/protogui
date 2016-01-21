module DataModel where

import Field exposing(Action)
import StructureDecoder exposing(Field, DataType, DataModel) 
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)   
import Signal exposing (Address)
import String exposing(toLower)

type Action =   NoOp
                | AddField String
                | RemoveField String
                | ModifyField String Field.Action

type alias Model = DataModel

basicTypes : List String
basicTypes =
    ["String", "Int"]

newField : String -> Field
newField kind' = 
    {
        name = "New field",
        kind = kind',
        repeated = False
    }

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        AddField name ->
            model
            --let addField field =

            --in
            --{model | datatypes}
        RemoveField name ->
            --fix
            model
        ModifyField name value ->
            model

view : Address Action -> DataModel -> Html
view address model =
    div[] <| List.map ( checkDataType address model ) model.dataTypes 


checkDataType : Address Action -> DataModel -> DataType -> Html
checkDataType address model datatype =
    if datatype.name == model.root then 
        div[]
        [
             h1[][ text datatype.name ]
            , div[] <| List.map ( displayField address model ) datatype.fields
        ]
    else
        div[][]

displayField : Address Action -> DataModel -> Field -> Html
displayField address model field =                    -- play around with required attribute
    if List.member field.kind basicTypes then
        if field.repeated then
            -- display field but with add/remove button. another index to keep track ? if 0 cant remove? or just leave

                div[]
                [
                    (Field.view (Signal.forwardTo address (ModifyField field.name)) field)
                    , button[ onClick address <| AddField field.name] [text "Add/ remove"]
                ]
        else
            div[][ (Field.view (Signal.forwardTo address (ModifyField field.name)) field) ]
            -- display without add/ remove buttons
    else -- Not a basic type, I.e an interface type, therefore need to get the list of fields that represent an interface object.
        div[] 
        [
              (Field.view (Signal.forwardTo address (ModifyField field.name)) field)
            , div[] <| List.map ( displayField address model ) ( getDataType  field.kind model )
        ]


fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "Error: fromJust nothing (empty JSON string?)"

getDataType : String -> DataModel -> List Field
getDataType kind model =
    -- lambda function to filter out only datatypes that are of the kind specified
    -- I.e to get the interface datatype.

    -- Then take the head of that list of datatypes that is returned, and get the value from it (List.head returns a "Maybe a", because the list
    -- could be empty, but we know it won't be).
    (fromJust <| List.head <| (List.filter (\datatype -> datatype.name == kind ) model.dataTypes)).fields 
                                                                               