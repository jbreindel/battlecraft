
module TmxMap exposing (TmxTileLayer,
                        TmxObject,
                        TmxObjectLayer,
                        TmxLayer(..),
                        TmxTileSet,
                        TmxMap,
                        tmxMap,
                        MinTmxMap,
                        minTmxMap,
                        tmxMapHeight,
                        tmxMapWidth)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)

-- Model

type alias TmxTileLayer = {
    name : String,
    data : List Int,
    height : Int,
    width : Int,
    visible : Bool,
    x : Int,
    y : Int,
    opacity : Int
}

tmxTileLayer : Decoder TmxTileLayer
tmxTileLayer =
    succeed TmxTileLayer
        |: ("name" := string)
        |: ("data" := list int)
        |: ("height" := int)
        |: ("width" := int)
        |: ("visible" := bool)
        |: ("x" := int)
        |: ("y" := int)
        |: ("opacity" := int)

type alias TmxObject = {
    id : Int,
    height : Int,
    width : Int,
    rotation : Int,
    visible : Bool,
    x : Int,
    y : Int
}

tmxObject : Decoder TmxObject
tmxObject =
    succeed TmxObject
        |: ("id" := int)
        |: ("height" := int)
        |: ("width" := int)
        |: ("rotation" := int)
        |: ("visible" := bool)
        |: ("x" := int)
        |: ("y" := int)

type alias TmxObjectLayer = {
    name : String,
    objects : List TmxObject,
    height : Int,
    width : Int,
    opacity : Int,
    x : Int,
    y : Int
}

tmxObjectLayer : Decoder TmxObjectLayer
tmxObjectLayer =
    succeed TmxObjectLayer
        |: ("name" := string)
        |: ("objects" := list tmxObject)
        |: ("height" := int)
        |: ("width" := int)
        |: ("opacity" := int)
        |: ("x" := int)
        |: ("y" := int)

type TmxLayer =
    TmxTlLayer TmxTileLayer |
    TmxObjLayer TmxObjectLayer

tmxLayerInfo : String -> Decoder TmxLayer
tmxLayerInfo layerType =
    case layerType of

        "tilelayer" ->
            object1 TmxTlLayer tmxTileLayer

        "objectgroup" ->
            object1 TmxObjLayer tmxObjectLayer

        lyrType ->
            Debug.crash lyrType

tmxLayer : Decoder TmxLayer
tmxLayer =
    at ["type"] string `andThen` tmxLayerInfo

type alias TmxTileSet = {
    name : String,
    firstGid : Int,
    image : String,
    imageHeight : Int,
    imageWidth : Int,
    columns : Int,
    margin : Int,
    spacing : Int,
    tileCount : Int,
    tileHeight : Int,
    tileWidth : Int
}

tmxTileSet : Decoder TmxTileSet
tmxTileSet =
    succeed TmxTileSet
        |: ("name" := string)
        |: ("firstgid" := int)
        |: ("image" := string)
        |: ("imageheight" := int)
        |: ("imagewidth" := int)
        |: ("columns" := int)
        |: ("margin" := int)
        |: ("spacing" := int)
        |: ("tilecount" := int)
        |: ("tileheight" := int)
        |: ("tilewidth" := int)

type alias TmxMap = {
    height : Int,
    width : Int,
    tileHeight : Int,
    tileWidth : Int,
    layers : List TmxLayer,
    tileSets : List TmxTileSet
}

tmxMap : Decoder TmxMap
tmxMap =
    succeed TmxMap
        |: ("height" := int)
        |: ("width" := int)
        |: ("tileheight" := int)
        |: ("tilewidth" := int)
        |: ("layers" := list tmxLayer)
        |: ("tilesets" := list tmxTileSet)

type alias MinTmxMap = {
    height : Int,
    width : Int,
    tileHeight : Int,
    tileWidth : Int
}

minTmxMap : Decoder MinTmxMap
minTmxMap =
    succeed MinTmxMap
        |: ("height" := int)
        |: ("width" := int)
        |: ("tileheight" := int)
        |: ("tilewidth" := int)

-- Helper funs

tmxMapHeight : MinTmxMap -> Int
tmxMapHeight minTmxMap =
    minTmxMap.height * minTmxMap.tileHeight

tmxMapWidth : MinTmxMap -> Int
tmxMapWidth minTmxMap =
    minTmxMap.width * minTmxMap.tileWidth
