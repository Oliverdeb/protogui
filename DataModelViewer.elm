module DataModelViewer where

--import Field exposing(Action)
import StructureDecoder exposing(Field, DataType, DataModel)
import ValueParser exposing(Data, getValues, dataToList, dataToDict, dataToString, newList, newString, newDict, isNull, newNull)
import Html exposing(..)
import Html.Events exposing(..)
import Html.Lazy exposing (lazy2)
import Html.Attributes exposing (..)
import Signal exposing (Address, message)
import String exposing(toLower)
import Array exposing (..)
import Dict

type Action =   NoOp
                | AddField FieldPath
                | AddType FieldPath
                | RemoveField FieldPath
                | UpdateFieldValue FieldPath String

type alias Model =     {
          datamodel : DataModel
        , values : Data
    }

type FieldSegment =
    FieldIndex Int
    | FieldName String

mustFieldName : FieldSegment -> String
mustFieldName segment =
    case segment of
        (FieldName name) -> name
        _ -> Debug.crash "not a field name"

mustFieldIndex : FieldSegment -> Int
mustFieldIndex segment =
    case segment of
        (FieldIndex n) -> n
        _ -> -1

type alias FieldPath = List FieldSegment

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
        AddField path ->
            { model |
                values = fromJust (addField model.datamodel path (Maybe.Just model.values))
            }
        AddType path ->
            { model |
                values = fromJust (addType model.datamodel path model.datamodel.root (Maybe.Just model.values))
            }
        RemoveField path ->
            { model |
                values = fromJust (removeField model.datamodel path (Maybe.Just model.values))
            }
        UpdateFieldValue path value ->
            { model |
                values = fromJust (updateField model.datamodel path value (Maybe.Just model.values))
            }

getLeafType : DataModel -> DataType -> FieldPath -> String
getLeafType model datatype fieldpath =
    let
        head = fromJust (List.head fieldpath)
        fieldname = mustFieldName head
        field = getField datatype fieldname
    in
        if List.member field.kind basicTypes then
            field.kind
        else
            if (List.length fieldpath) == 1 then
                field.kind
            else
                if field.repeated then
                    getLeafType model (getDataType model field.kind) (List.drop 2 fieldpath)
                else
                    getLeafType model (getDataType model field.kind) (List.drop 1 fieldpath)

updateAt : DataModel -> DataType -> FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateAt model datatype fieldpath data fn =
    let
        fieldname = mustFieldName (fromJust (List.head fieldpath))
        isRepeated = (getField datatype fieldname).repeated
        dict = dataToDict data
        fieldpathtail = fromJust (List.tail fieldpath)
        fielddata = fromJust (Dict.get fieldname dict)
    in
        if (List.length fieldpath) == 1 then
            Maybe.Just (newDict (Dict.update fieldname (\fielddata -> (updateLeafSingle model datatype fieldpath (fromJust fielddata) fn)) dict))
        else -- ["Interfaces", "0", "IPs", "0"]   ----------- ["IPs", "0"], Interface,
            updateTree model datatype fieldpath data fn

updateLeafSingle : DataModel -> DataType -> FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateLeafSingle model datatype fieldpath data fn =
    let
        fieldname = mustFieldName (fromJust (List.head fieldpath))
        field = getField datatype fieldname
    in
        if field.repeated then
            fn (Maybe.Just data)
        else
            fn (Dict.get fieldname (dataToDict data))

updateLeafList : FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateLeafList fieldpath data fn = -- ["0"] -> [String "10.102.0.1", String "10.102.0.100"]
    let
        target = mustFieldIndex <| fromJust <| List.head fieldpath
        list = dataToList data
        array = Array.fromList list
        newValue = fn (Array.get target array)
        updatedArray = Array.set target (fromJust newValue) array
        result = Array.toList updatedArray
        nonNull = List.filter (\x -> not (isNull x)) result
    in
        --updateList list target fn
        Maybe.Just <| newList nonNull

updateList : List Data -> Int -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateList xs target fn =
    let
        indexed = List.indexedMap (,) xs
        updateIf (i, x) =
            if i == target then
                fromJust <| fn <| Maybe.Just x
            else
                x
    in
        Maybe.Just <| newList <| List.map updateIf indexed

updateTree : DataModel -> DataType -> FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateTree model datatype fieldpath data fn = -- ["Interfaces", "0", "IPs", "0"] -------- ["IPs", "0"] ---------- ["Subthing", "SubNicks"]
    let
        fieldname = mustFieldName <| fromJust <| List.head fieldpath
        isRepeated = (getField datatype fieldname).repeated
        dict = dataToDict data
        fieldpathtail = fromJust (List.tail fieldpath)
        fielddata = fromJust (Dict.get fieldname dict)
    in
        if isRepeated then
            if List.length fieldpathtail == 1 then
                Maybe.Just 
                <| newDict 
                <| Dict.update fieldname 
                (\fielddata -> (updateLeafList fieldpathtail (fromJust fielddata) fn)) dict
            else
                Maybe.Just 
                <| newDict 
                <|Dict.update fieldname 
                (\fielddata -> (updateTreeList fieldname model datatype fieldpathtail (fromJust fielddata) fn)) dict
        else
            updateTreeDict fieldname model datatype fieldpath fielddata fn

updateTreeList : String -> DataModel -> DataType -> FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateTreeList fieldname model datatype fieldpath data fn = -- ["0", "IPs", "0"]   ---- ["0"], List IPs
    let
        field = getField datatype fieldname
        childtype = getDataType model field.kind
        list = dataToList data
        target = mustFieldIndex (fromJust (List.head fieldpath))
        array = Array.fromList list
        updatedArray = Array.set target (fromJust (updateAt model childtype (fromJust (List.tail fieldpath)) (fromJust (Array.get target array)) fn)) array
        result = Array.toList updatedArray
    in
        Maybe.Just (newList result)

updateTreeDict : String -> DataModel -> DataType -> FieldPath -> Data -> ((Maybe Data) -> (Maybe Data)) -> Maybe Data
updateTreeDict fieldname model datatype fieldpath data fn = -- ["SubThing", "SubNicks"], Server ---------- ["Hostname"]
    let
        field = getField datatype fieldname
        childtype = getDataType model field.kind
        dict = dataToDict data
        result = Dict.update fieldname (\child -> (updateAt model childtype (fromJust (List.tail fieldpath)) (fromJust child) fn )) dict
    in
        Maybe.Just (newDict result)

--removeType : DataModel -> FieldPath -> (Maybe Data) -> (Maybe Data)
--removeType model fieldpath maybeData =
--    let
--        data = fromJust maybeData
--    in
--        if (List.length fieldpath) == 2 then -- [ Data ], [ "SubNicks", "0" ]
--            -- use a case here
--            Maybe.Just (newList (List.map (\(i, datum) -> datum ) (List.filter (\(i, datum) -> i /= (mustToInt (secondEl fieldpath))) (List.indexedMap (,) (dataToList data)))))
--        else -- Data, [ "SubThings", "SubNicks", "0" ]
--            Maybe.Just (newDict (Dict.update (mustFieldName (secondEl fieldpath)) (removeField model (fromJust (List.tail fieldpath))) (dataToDict data)))

removeField : DataModel -> FieldPath -> (Maybe Data) -> (Maybe Data)
removeField model fieldpath maybeData =
    let
        data = fromJust maybeData
        datatype = getDataType model model.root
        leafType = getLeafType model datatype fieldpath
    in
        updateAt model datatype fieldpath data (removeFieldFn)
       
removeFieldFn : Maybe Data -> Maybe Data
removeFieldFn _ = Maybe.Just newNull

updateField : DataModel -> FieldPath -> String -> (Maybe Data) -> (Maybe Data)
updateField model fieldpath value maybeData =
    let
        data = fromJust maybeData
        datatype = getDataType model model.root
        leafType = getLeafType model datatype fieldpath
    in
        updateAt model datatype fieldpath data (updateFieldFn leafType value)

updateFieldFn : String -> String -> (Maybe Data) -> (Maybe Data)
updateFieldFn kind value maybeData =
        case kind of
            "String" -> Maybe.Just (newString value)
            _ -> maybeData

updateIfN : Int -> Int -> a -> a -> a
updateIfN index replace old new =
    if index == replace then
        new
    else
        old

addField : DataModel -> FieldPath -> (Maybe Data) -> (Maybe Data)
addField model fieldpath maybeData =
    let
        data = fromJust maybeData
        datatype = getDataType model model.root
        leafType = getLeafType model datatype fieldpath
    in
        updateAt model datatype fieldpath data (addFieldFn leafType)

addFieldFn : String -> (Maybe Data) -> (Maybe Data)
addFieldFn kind maybeData =
    let
        data = fromJust maybeData
        list = dataToList data
    in
        case kind of
            "String" -> Maybe.Just (newList (list ++ [newString ""]))
            _ -> maybeData

addType : DataModel -> FieldPath -> String -> (Maybe Data) -> (Maybe Data)
addType model fieldpath kind maybeData =
    let
        data = fromJust maybeData
        datatype = getDataType model model.root
        leafType = getLeafType model datatype fieldpath
    in
        updateAt model datatype fieldpath data (addTypeFn model leafType)

addTypeFn : DataModel -> String -> (Maybe Data) -> (Maybe Data)
addTypeFn model kind maybeData =
    let
        data = fromJust maybeData
        list = dataToList data
    in
        Maybe.Just (newList (list ++ [newDataType model kind]))

secondEl : List a -> a
secondEl xs = nthEl xs 2

thirdEl : List a -> a
thirdEl xs = nthEl xs 3

nthEl : List a -> Int -> a
nthEl xs n = fromJust (Array.get n (Array.fromList xs))

view : Address Action -> Model -> Html
view address model =
    let
        rootType = getDataType model.datamodel model.datamodel.root
    in
        displayDataType address model.datamodel rootType [] model.values

displayDataType : Address Action -> DataModel -> DataType -> FieldPath -> Data -> Html
displayDataType address datamodel datatype parentpath data =
    let
        heading = [ h1[] [ text datatype.name ] ]
    in
        div [] (heading ++ (displayFields address datamodel datatype data parentpath))

displayFields : Address Action -> DataModel -> DataType -> Data -> FieldPath -> List Html
displayFields address datamodel datatype data parentpath =
    List.map (displayField address datamodel data parentpath) datatype.fields

displayField : Address Action -> DataModel -> Data -> FieldPath -> Field -> Html
displayField address datamodel data parentpath field =
    let
        dataDict = (dataToDict data)
        encoded = Dict.get field.name dataDict
        shouldDisplay = isJust encoded
    in
        if List.member field.kind basicTypes then
            if field.repeated then
                displayRepeatedBasicType address field parentpath shouldDisplay encoded
            else
                displaySingleBasicType address field parentpath shouldDisplay encoded
        else 
            if field.repeated then
                displayRepeatedStruct address field datamodel parentpath shouldDisplay encoded
            else
                displaySingleStruct address field datamodel parentpath shouldDisplay encoded

displayRepeatedStruct : Address Action -> Field -> DataModel -> FieldPath -> Bool -> Maybe Data -> Html
displayRepeatedStruct address field datamodel parentpath shouldDisplay data =
    if shouldDisplay then
        div [] <| List.indexedMap (\ index value -> (displayDataType address datamodel (getDataType datamodel field.kind) 
                    (parentpath ++ [FieldName field.name] ++ [FieldIndex index])) value) (dataToList (fromJust data)) ++
           [ button [ onClick address (AddType (parentpath ++ [FieldName field.name]))] [ text ("Add " ++ field.name) ] ]
    else
        div [] [ button [ onClick address (AddType (parentpath ++ [FieldName field.name]))] [ text ("Add " ++ field.name) ] ]


displaySingleStruct : Address Action -> Field -> DataModel -> FieldPath -> Bool -> Maybe Data -> Html
displaySingleStruct address field datamodel parentpath shouldDisplay data =
    let
        datatype = getDataType datamodel field.kind
    in
        if shouldDisplay then
            displayDataType address datamodel datatype (parentpath ++ [FieldName field.name]) (fromJust data)
        else
            div [] (
                [ span [] [ text field.name ] ] ++
                    [ button [ onClick address (AddType (parentpath ++ [FieldName field.name])) ] [ text ("Set " ++ field.name) ] ]
            )

displayRepeatedBasicType : Address Action -> Field -> FieldPath -> Bool -> Maybe Data -> Html
displayRepeatedBasicType address field parentpath shouldDisplay data =
    if shouldDisplay then
        case field.kind of
            "String" -> div [] (
                [ span [] [ text field.name ] ] ++
                    (List.indexedMap (repeatedString address (parentpath ++ [FieldName field.name])) (dataToList (fromJust data))) ++
                        [ button [ onClick address (AddField (parentpath ++ [FieldName field.name])) ] [ text ("Add " ++ field.name) ] ]
            )
            _ -> div [] [ text "huh1" ]
    else
        case field.kind of
            "String" -> div [] (
                [ span [] [ text field.name ] ] ++
                        [ button [ onClick address (AddField (parentpath ++ [FieldName field.name])) ] [ text ("Add " ++ field.name) ] ]
            )
            _ -> div [] [ text "huh1" ]


displaySingleBasicType : Address Action -> Field -> FieldPath -> Bool -> Maybe Data -> Html
displaySingleBasicType address field parentpath shouldDisplay data =
    if shouldDisplay then
        case field.kind of
            "String" -> div [] [ span [] [ text field.name ] , singleString address (parentpath ++ [FieldName field.name]) (fromJust data) ]
            _ -> div [] [ text "huh2" ]
    else
        case field.kind of
            "String" -> div [] [ span [] [ text field.name ] , singleString address (parentpath ++ [FieldName field.name]) (newString "") ]
            _ -> div [] [ text "huh2" ]

repeatedString : Address Action -> FieldPath -> Int -> Data -> Html
repeatedString address fieldpath index data = div [] [input [ value (dataToString data)
                                                                , on "input" targetValue ( Signal.message address << (UpdateFieldValue (fieldpath ++ [FieldIndex index]))) ] [] 
                                                                , button [ onClick address (RemoveField (fieldpath ++ [FieldIndex index])) ]  [ text "Remove" ] ]

singleString : Address Action -> FieldPath -> Data -> Html
singleString address fieldpath data = div [] [input [ value (dataToString data) , (on "input" targetValue ( Signal.message address << (UpdateFieldValue fieldpath))) ] [] ]

isJust : Maybe a -> Bool
isJust x = case x of
    Just y -> True
    _ -> False

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "Error: fromJust nothing (empty JSON string?)"

getDataType : DataModel -> String -> DataType
getDataType model kind =
    let
        x =     (fromJust <| List.head <| (List.filter (\datatype -> datatype.name == kind ) model.dataTypes))
    in
        x

getDataTypeForField : DataModel -> String -> String -> DataType
getDataTypeForField model kind fieldname =
    let
        datatype = getDataType model kind
    in
        getDataType model (getField datatype fieldname).kind

getField : DataType -> String -> Field
getField datatype fieldname =
    let
        firstHit fn xs = fromJust (List.head (List.filter fn xs))
        isField field = field.name == fieldname
    in
        firstHit isField datatype.fields


mustToInt : String -> Int
mustToInt s = case (String.toInt s) of
    Ok d -> d
    Err err -> Debug.crash "Very bad"

newDataType : DataModel -> String -> Data
newDataType model kind =
    let
        datatype = getDataType model kind
        fields = List.filter (\field -> List.member field.kind basicTypes) datatype.fields
        maplist = List.map (\field -> (field.name, newBasicDataType field.kind)) fields
    in
    newDict (Dict.fromList maplist)

newBasicDataType : String -> Data
newBasicDataType kind =
    case kind of
        "String" -> newString ""
        _ -> newString ""
