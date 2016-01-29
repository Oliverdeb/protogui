module ValueParser (getValues, dataToDict, dataToString, dataToList, Data, newArray, newString, newList, newDict, isNull, newNull) where

import Json.Decode as Js exposing (Decoder, (:=))
import Dict exposing (Dict)
import Array exposing (Array)

import Graphics.Element exposing (show)

type Data
  = Object (Dict String Data)
  | Arr (Array Data)
  | String String
  | Int Int
  | Float Float
  | Bool Bool
  | Null

newArray : Array Data -> Data
newArray xs = Arr xs

newList : List Data -> Data
newList xs = Arr (Array.fromList xs)

newString : String -> Data
newString s = String s

newDict : Dict String Data -> Data
newDict x = Object x

newNull : Data
newNull = Null

isNull : Data -> Bool
isNull x =
  case x of
    Null -> True
    _ -> False

-- decoder : Decoder Data
decoder () =
  Js.oneOf
    [ object (),
      array (),
      string,
      int,
      float,
      bool,
      null
    ]

-- object : Decoder Data
object () =
  -- Js.dict decoder |> Js.map Object
  -- One level of indirection like this is only enough to allow one level of object/array nesting:
  --
  Js.dict (Js.succeed True) `Js.andThen` (\_ -> Js.dict (decoder ()) |> Js.map Object)
  --
  -- You can also get one level of nesting by defining a local decoder
  -- identical to the module-level definition:
  --
  -- let
  --   decoderCopy =
  --     Js.oneOf
  --       [ object,
  --         array,
  --         string,
  --         int,
  --         float,
  --         bool,
  --         null
  --       ]
  -- in
  --   Js.dict decoderCopy |> Js.map Object

-- array : Decoder Data
array () =
  -- Js.array decoder |> Js.map Array
  -- Same situation as with object decoder above, e.g. this gives you one level of nesting:
  Js.array (Js.succeed True) `Js.andThen` (\_ -> Js.array (decoder ()) |> Js.map Arr)
  --
  -- let
  --   decoderCopy =
  --     Js.oneOf
  --       [ object,
  --         array,
  --         string,
  --         int,
  --         float,
  --         bool,
  --         null
  --       ]
  -- in
  --   Js.array decoderCopy |> Js.map Array

string : Decoder Data
string =
  Js.string |> Js.map String

int : Decoder Data
int =
  Js.int |> Js.map Int

float : Decoder Data
float =
  Js.float |> Js.map Float

bool : Decoder Data
bool =
  Js.bool |> Js.map Bool

null : Decoder Data
null =
  Js.null Null

--main =
--  case moreJSON |> Js.decodeString (decoder ()) of
--    Ok value -> show value
--    Err msg -> show msg
--  --moreJSON
--  --|> Js.decodeString (decoder ())
--  --|> show

--main =
--  let res =  Js.decodeString (decoder ()) moreJSON
--  in
--  case res of
--    Ok value -> show value
--    Err msg -> show msg

someJson : String
someJson = """
{"a":[1,2,3], "b" : 3}
"""

moreJSONWithInterfaces = """
{
  "Hostname": "server01",
  "Interfaces": [
    {"Name": "eth0", "MAC": "00:11:22:33:44:55", "IPs": ["10.1.1.1", "10.1.2.2"]}
  ]
}
"""

demo = """
{
  "Hostname": "server01",
  "Nicknames": [ "foo", "bar" ],  
  "Interfaces": [
    {"Name": "eth0", "MAC": "00:11:22:33:44:55", "IPs": ["10.1.1.1", "10.1.2.2"]}
  ]
}
"""

moreJSON = """
{
  "Hostname": "server01",
  "Nicknames": [ "foo", "bar" ],
  "SubThing": { "SubName": "subby", "SubNicks": ["awesome", "cool"] },
  "Interfaces": [
    {"Name": "eth0", "MAC": "00:11:22:33:44:55", "IPs": ["10.1.1.1", "10.1.2.2"]},
    {"Name": "eth1", "MAC": "00:11:22:33:44:55", "IPs": ["10.1.1.1", "10.1.2.2"]}
  ]
}
"""



-- comment out everything below and main will run and you can see the output of the json parser.

getValues : Data
getValues =
  let parsed =  Js.decodeString (decoder ()) moreJSON
  in
  case parsed of
    Ok value ->  value
    Err msg -> Object(Dict.empty )

dataToDict : Data -> Dict String Data
dataToDict data =
  case data of
    Object inner -> inner
    _ -> Dict.empty

dataToList : Data -> List Data
dataToList data =
  case data of
    (Arr xs) -> Array.toList xs
    _ -> []

dataToString : Data -> String
dataToString data =
  case data of
    String str -> str
    _ -> ""
