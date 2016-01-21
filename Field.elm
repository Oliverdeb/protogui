module Field (Model, Action, update, view) where

import StructureDecoder exposing (Field)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.Lazy exposing(lazy2)
import Signal exposing(Address)

type alias Model = Field

type Action = NoOp 
            | UpdateFieldValue String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model
    UpdateFieldValue name ->
        model

view : Address Action -> Model -> Html
view address model =
    div[][
        text <| model.name ++ ": "
        , input[placeholder <|  "Enter " ++ model.name ++ "here..." ][]
        ]
