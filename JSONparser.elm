
{-}
"""
{
  "Hostname": "server01",
  "Interfaces": [
    {"Name": "eth0", "MAC": "00:11:22:33:44:55", IPs: ["10.1.1.1", "10.1.2.2"]}
  ]
}
"""

-}


import Json.Decode as Js exposing (Decoder, (:=))
import Dict exposing (Dict)
import Array exposing (Array)

import Graphics.Element exposing (show)

type Data
  = Object (Dict String Data)
  | Array (Array Data)
  | String String
  | Int Int
  | Float Float
  | Bool Bool
  | Null

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
  Js.array (Js.succeed True) `Js.andThen` (\_ -> Js.array (decoder ()) |> Js.map Array)
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

main =
  someJson
  |> Js.decodeString (decoder ())
  |> show

someJson : String
someJson = """
{"a":[1,2,3], "b" : 3}
"""
