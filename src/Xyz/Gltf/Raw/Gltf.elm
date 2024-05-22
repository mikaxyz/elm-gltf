module Xyz.Gltf.Raw.Gltf exposing
    ( Gltf
    , Asset
    , decoder
    , decoderWithSingleBuffer
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format

@docs Gltf
@docs Asset
@docs decoder
@docs decoderWithSingleBuffer

-}

import Array exposing (Array)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Xyz.Gltf.Accessor as Accessor exposing (Accessor)
import Xyz.Gltf.Animation as Animation exposing (Animation)
import Xyz.Gltf.Buffer as Buffer exposing (Buffer)
import Xyz.Gltf.BufferView as BufferView exposing (BufferView)
import Xyz.Gltf.Mesh as Mesh exposing (Mesh)
import Xyz.Gltf.Node as Node exposing (Node)
import Xyz.Gltf.Scene as Scene exposing (Scene)
import Xyz.Gltf.Skin as Skin exposing (Skin)


{-| Type representing raw data from a .gltf/.glb file
-}
type alias Gltf =
    { asset : Asset
    , scenes : Array Scene
    , nodes : Array Node
    , meshes : Array Mesh
    , skins : Array Skin
    , buffers : Array Buffer
    , bufferViews : Array BufferView
    , accessors : Array Accessor
    , animations : Array Animation
    }


{-| Information about the file
-}
type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


{-| Decoder
-}
decoder : JD.Decoder Gltf
decoder =
    JD.succeed Gltf
        |> JDP.required "asset" assetDecoder
        |> JDP.required "scenes" (JD.array Scene.decoder)
        |> JDP.required "nodes"
            (JD.list JD.value
                |> JD.map
                    (\values ->
                        values
                            |> List.indexedMap
                                (\index value ->
                                    value
                                        |> JD.decodeValue (Node.decoder index)
                                        |> Result.toMaybe
                                )
                            |> List.filterMap identity
                            |> Array.fromList
                    )
            )
        |> JDP.required "meshes" (JD.array Mesh.decoder)
        |> JDP.optional "skins" (JD.array Skin.decoder) Array.empty
        |> JDP.required "buffers" (JD.array Buffer.decoder)
        |> JDP.required "bufferViews" (JD.array BufferView.decoder)
        |> JDP.required "accessors" (JD.array Accessor.decoder)
        |> JDP.optional "animations" (JD.array Animation.decoder) Array.empty


{-| Decoder for when buffer data exists
-}
decoderWithSingleBuffer : Buffer -> JD.Decoder Gltf
decoderWithSingleBuffer buffer =
    JD.succeed Gltf
        |> JDP.required "asset" assetDecoder
        |> JDP.required "scenes" (JD.array Scene.decoder)
        |> JDP.required "nodes"
            (JD.list JD.value
                |> JD.map
                    (\values ->
                        values
                            |> List.indexedMap
                                (\index value ->
                                    value
                                        |> JD.decodeValue (Node.decoder index)
                                        |> Result.toMaybe
                                )
                            |> List.filterMap identity
                            |> Array.fromList
                    )
            )
        |> JDP.required "meshes" (JD.array Mesh.decoder)
        |> JDP.optional "skins" (JD.array Skin.decoder) Array.empty
        |> JDP.hardcoded ([ buffer ] |> Array.fromList)
        |> JDP.required "bufferViews" (JD.array BufferView.decoder)
        |> JDP.required "accessors" (JD.array Accessor.decoder)
        |> JDP.optional "animations" (JD.array Animation.decoder) Array.empty


assetDecoder : JD.Decoder Asset
assetDecoder =
    JD.map3 Asset
        (JD.field "version" JD.string)
        (JD.field "copyright" JD.string |> JD.maybe)
        (JD.field "generator" JD.string |> JD.maybe)
