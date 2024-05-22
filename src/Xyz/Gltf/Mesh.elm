module Xyz.Gltf.Mesh exposing
    ( Attribute(..)
    , Index(..)
    , Mesh
    , Primitive
    , decoder
    , indexDecoder
    )

import Dict
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Xyz.Gltf.Accessor as Accessor
import Xyz.Gltf.Util as Util


type Index
    = Index Int


type alias Mesh =
    { primitives : List Primitive
    }


type alias Primitive =
    { attributes : List Attribute
    , indices : Maybe Accessor.Index
    }


type Attribute
    = Position Accessor.Index
    | Normal Accessor.Index
    | Joints Int Accessor.Index
    | Weights Int Accessor.Index
    | Unknown String


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Mesh
decoder =
    JD.map Mesh
        (JD.field "primitives" (JD.list primitiveDecoder))


primitiveDecoder : JD.Decoder Primitive
primitiveDecoder =
    JD.succeed Primitive
        |> JDP.required "attributes" attributesDecoder
        |> JDP.optional "indices" (JD.maybe Accessor.indexDecoder) Nothing


attributesDecoder : JD.Decoder (List Attribute)
attributesDecoder =
    JD.dict Accessor.indexDecoder |> JD.map (Dict.map toAttribute >> Dict.values)


toAttribute : String -> Accessor.Index -> Attribute
toAttribute key accessorIndex =
    case key of
        "POSITION" ->
            Position accessorIndex

        "NORMAL" ->
            Normal accessorIndex

        _ ->
            case String.split "_" key of
                "JOINTS" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> Joints index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                "WEIGHTS" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> Weights index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                _ ->
                    Unknown key
