module Main where

import Characteristics
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)
import Signal exposing (Address)
import String exposing(toLower)

type alias Model = 
    {
        index : Int,
        servers : List Server
    }

type alias Server = 
    {
        model : Characteristics.Model,
        name : String,
        id : Int
    }

type Action =
    RemoveServer Int
    | AddServer 
    | ModifyServer Int Characteristics.Action
    | NoOp

newServer : Int -> Server
newServer n =
    {
        model = Characteristics.init,
        name = "Server " ++ toString n,
        id = n
    }

emptyModel : Model
emptyModel = 
    {
        index = 1,
        servers = []
    }

update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        AddServer ->
            { model |   index = model.index + 1,
                        servers =  model.servers ++ [newServer model.index] }

        RemoveServer n ->
            { model | servers = List.filter (\s -> s.id /= n) model.servers }

        ModifyServer n action ->
            let updateServer server =
                if server.id == n then
                    { server | model = Characteristics.update  action server.model }
                else
                    server
            in 
            { model | servers = List.map updateServer model.servers }

view : Address Action -> Model -> Html
view address model = 
    --let server = 
    --    (List.map showServer model.server)

    --in
      --  div[] ( )
    div[]
    [
        lazy2 showServers address model.servers,
        br[][],
        button[ onClick address AddServer ][ text "Add Server" ]
        
    ]

showServers : Address Action -> List Server -> Html
showServers address servers = 
    div[] <| List.map (showServer address) servers
{-


showServers : Address Action -> List Server -> Html
showServers address servers =
    div[] <| List.map (showServer address) servers

-}


showServer : Address Action -> Server -> Html
showServer address server =
    div[]
    [
        h1[][text server.name ],
        (Characteristics.view (Signal.forwardTo address (ModifyServer server.id)) server.model),
        button [ onClick address <| RemoveServer server.id ] [ text <| "Remove " ++ toLower server.name ],
        br[][]
    ]

model : Signal Model
model =
    Signal.foldp update emptyModel actions.signal

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


main : Signal Html
main = 
    Signal.map (view actions.address) model