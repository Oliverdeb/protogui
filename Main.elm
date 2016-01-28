module Main  where

-- IMPLEMENT LAZY.
-- dictionary of actual values - updating of values using dict?
-- passing actions through to the lower models.
import StructureDecoder exposing (DataModel, getModel)
import ValueParser exposing (Data, getValues)
import DataModelViewer
import Dict exposing (Dict)
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)
import Signal exposing (Address)

type Action =
    ModifyModel DataModelViewer.Action
    | NoOp

type alias Model = DataModelViewer.Model

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        ModifyModel action ->
             DataModelViewer.update action model

view : Address Action -> Model -> Html
view address model =
    div[]
    [
          (DataModelViewer.view (Signal.forwardTo address ModifyModel) model)
    ]

initialDataModel : Model
initialDataModel =
    {
          datamodel = getModel
        , values = getValues
    }

model : Signal Model
model =
    Signal.foldp update initialDataModel actions.signal

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


main : Signal Html
main =
    Signal.map (view actions.address) model
