module Map exposing (..)

import Effects exposing (Effects)

-- Model

type alias TileLayer = {
    name : String,
    data : List Int,
    height : Int,
    width : Int,
    visible : Bool,
    x : Int,
    y : Int,
    opacity : Int
}

type alias Object = {
    id : Int,
    height : Int,
    width : Int,
    rotation : Int,
    visible : Bool,
    x : Int,
    y : Int
}

type alias ObjectLayer = {
    name : String,
    objects : List Object,
    height : Int,
    width : Int,
    opacity : Int,
    x : Int,
    y : Int
}

type alias TileSet = {
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

type alias Map = {
    height : Int,
    width : Int,
    tileHeight : Int,
    tileWidth : Int,
    tileLayers : List TileLayer,
    objectLayers : List ObjectLayer,
    tileSets : List TileSet
}

type alias Model = {
    map : Map
}
