module Decoder where


import Graphics.Element exposing(Element, flow, down, show)
import Json.Decode exposing(..) 


test = "{   \"age\": 42 }"


age : Decoder Int
age = 
    "age" := int


main = show <| decodeString age test
