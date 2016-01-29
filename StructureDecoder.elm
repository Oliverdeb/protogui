module StructureDecoder (getModel, DataModel, DataType, Field) where

import Debug
import Graphics.Element exposing (show)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, (:=), succeed, object2, list,string, bool, dict, int, maybe, decodeString, map)

type alias DataModel = {
     dataTypes : List DataType
     , root : String
}

type alias DataType = {
     name : String
     , fields : List Field
}

type alias Field = {
     name : String
     , kind : String
     , repeated : Bool
}

model : Model
model = emptyModel

emptyModel : DataModel
emptyModel =
    {
          dataTypes = []
        , root = ""
    }

type alias Model = DataModel

  -- Applicative’s `pure`:
constructing : a -> Decoder a
constructing = succeed

  -- Applicative’s `<*>`:
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply = object2 (<|)

dataModelDecoder : Decoder DataModel
dataModelDecoder =
     constructing DataModel
     `apply` ("dataTypes" := dataTypesDecoder)
     `apply` ("root" := string)

dataTypesDecoder : Decoder (List DataType)
dataTypesDecoder = list dataTypeDecoder

dataTypeDecoder : Decoder DataType
dataTypeDecoder =
  constructing DataType
  `apply` ("name" := string)
  `apply` ("fields" := fieldsDecoder)

fieldsDecoder : Decoder (List Field)
fieldsDecoder = list fieldDecoder

fieldDecoder : Decoder Field
fieldDecoder =
     constructing Field
     `apply` ("name" := string)
     `apply` ("kind" := string)
     `apply` ("repeated" := bool)

jsonString = """
{
     "root": "Server",
     "dataTypes": [
         {
             "name": "Server",
             "fields": [
                 {"name": "Hostname", "kind": "String", "repeated": false},
                 {"name": "Nicknames", "kind": "String", "repeated": true},
                 {"name": "Interfaces", "kind": "Interface", "repeated": true},
                 {"name": "SubThing", "kind": "SubServer", "repeated": false}
             ]
         },

         {
             "name": "Interface",
             "fields": [
                 {"name": "Name", "kind": "String", "repeated": false},
                 {"name": "MAC", "kind": "String", "repeated": false},
                 {"name": "IPs", "kind": "String", "repeated": true}
             ]
         },

         {
             "name": "SubServer",
             "fields": [
                 {"name": "SubName", "kind": "String", "repeated": false},
                 {"name": "SubNicks", "kind": "String", "repeated": true}
             ]
         }
     ]
}
"""

demoJSON = """
{
     "root": "Server",
     "dataTypes": [
         {
             "name": "Server",
             "fields": [
                 {"name": "Hostname", "kind": "String", "repeated": false},
                 {"name": "Nicknames", "kind": "String", "repeated": true},
                 {"name": "Interfaces", "kind": "Interface", "repeated": true}                 
             ]
         },

         {
             "name": "Interface",
             "fields": [
                 {"name": "Name", "kind": "String", "repeated": false},
                 {"name": "MAC", "kind": "String", "repeated": false},
                 {"name": "IPs", "kind": "String", "repeated": true}
             ]
         }
     ]
}
"""

getModel : DataModel
getModel =
    case (decodeString dataModelDecoder jsonString    ) of
        Ok value ->  setModel value
        Err msg  ->  setModel emptyModel

setModel : DataModel -> DataModel
setModel value =
    { model |   root = value.root,
                dataTypes = value.dataTypes }


--main =
--  case (decodeString dataModelDecoder jsonString) of
--      Ok value -> show value
--      Err msg -> show msg
