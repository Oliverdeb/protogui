module Main  where 

-- IMPLEMENT LAZY.
-- dictionary of actual values - updating of values using dict? 
-- passing actions through to the lower models.
import StructureDecoder exposing (DataModel)
import DataModel
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)   
import Signal exposing (Address)

type Action =
    RemoveModel Int
    | AddModel
    | ModifyModel Int DataModel.Action
    | NoOp

type alias IndexedDataModel = 
    {
        datamodel : StructureDecoder.DataModel
        , id : Int
    }

type alias Model = 
    {
        datamodels : List IndexedDataModel
        , index : Int
    }

newDataModel : Int -> IndexedDataModel 
newDataModel index =
    {
          datamodel = StructureDecoder.getModel
        , id = index

    }

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        AddModel ->
            { model | index = model.index + 1
                    , datamodels = model.datamodels ++ [ newDataModel model.index ] }   

        RemoveModel id ->  
            if id /= 0 then
                { model | datamodels = List.filter (\datamodel -> datamodel.id /= id) model.datamodels }
            else 
                model

        ModifyModel id action -> -- pass modify action etc
            model     

view : Address Action -> Model -> Html
view address model = 
    div[]( List.map ( displayModel address ) model.datamodels )


displayModel : Address Action -> IndexedDataModel -> Html
displayModel address datamodel =
    div[] 
    [
        (DataModel.view (Signal.forwardTo address (ModifyModel datamodel.id)) datamodel.datamodel)
        , button[ onClick address AddModel ] [ text <| "New " ++ datamodel.datamodel.root ]
        , button[ onClick address (RemoveModel datamodel.id) ] [ text <|  "Remove " ++ datamodel.datamodel.root ]
    ] 

initiallDataModel : IndexedDataModel
initiallDataModel = 
    {
          datamodel = StructureDecoder.getModel
        , id = 0
    }

initialModel : Model
initialModel = 
    {
        datamodels = [initiallDataModel]
      , index = 1
    }

model : Signal Model
model =
    Signal.foldp update initialModel actions.signal

actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


main : Signal Html
main =
    Signal.map (view actions.address) model