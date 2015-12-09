
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.Lazy exposing(lazy2)
import Signal exposing(Address)


-- MODEL

type alias Model = {
        numOfInputs : Int,
        data : List Data    
    }

type alias Data = {
    value : String,
    id : Int
    }

-- UPDATE

type Action = NoOp | AddInput | RemoveInput Int | UpdateFieldValue Int String

emptyModel : Model
emptyModel = {
    numOfInputs = 0,
    data = []
    }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
        model
    AddInput ->
      { model | numOfInputs = model.numOfInputs + 1,
                data = model.data ++ [newData model.numOfInputs]
    }

    UpdateFieldValue id input ->
        let updateField data
            = if data.id == id then 
                { data | value = input} 
            else data
        in
            { model | data = List.map updateField model.data }

    RemoveInput id ->        
      {model | data = List.filter  (\d -> d.id /= id) model.data }



newData : Int -> Data
newData numOfInputs =
    {
        value = "",
        id = numOfInputs
    }

view : Address Action -> Model -> Html
view address model =
    div[]
    [
        h1[][text "Server"],
        section[id "server-config"]
        [
            lazy2 inputs address model.data,
            button[ onClick address AddInput ][text "Add server"]
        ]
    ]

inputs : Address Action -> List Data -> Html
inputs address data = 
    div[]( List.map (addInput address) data )

addInput : Address Action -> Data -> Html
addInput address data =
    div[][ 
        input 
        [
            value data.value,
            autofocus True,
            placeholder "Enter ip here...",
            name "Server input field",
            on "input" targetValue ( Signal.message address << UpdateFieldValue data.id )
        ]
        [],
        button [ onClick address <| RemoveInput data.id] [ text "Remove" ]
    ]


countStyle : Attribute
countStyle =
  style
    [{- ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")-}
    ]

model : Signal Model
model =
    Signal.foldp update emptyModel actions.signal

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

main = 
    Signal.map (view actions.address) model

