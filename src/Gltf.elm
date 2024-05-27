module Gltf exposing
    ( Gltf, Asset
    , Animation, Scene, Node, Accessor, Buffer, BufferView, Skin, Mesh
    , decoder, bytesDecoder
    , getBinary, getEmbedded
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format

@docs Gltf, Asset
@docs Animation, Scene, Node, Accessor, Buffer, BufferView, Skin, Mesh
@docs decoder, bytesDecoder
@docs getBinary, getEmbedded

-}

import Array exposing (Array)
import Bytes.Decode
import Gltf.Glb as Glb
import Http
import Internal.Accessor as Accessor
import Internal.Animation as Animation
import Internal.Buffer as Buffer
import Internal.BufferView as BufferView
import Internal.Mesh as Mesh
import Internal.Node as Node
import Internal.Scene as Scene
import Internal.Skin as Skin
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


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


{-| Raw Accessor data
-}
type alias Accessor =
    Accessor.Accessor


{-| Raw Animation data
-}
type alias Animation =
    Animation.Animation


{-| Raw Scene data
-}
type alias Scene =
    Scene.Scene


{-| Raw Node data
-}
type alias Node =
    Node.Node


{-| Raw Buffer data
-}
type alias Buffer =
    Buffer.Buffer


{-| Raw BufferView data
-}
type alias BufferView =
    BufferView.BufferView


{-| Raw Mesh data
-}
type alias Mesh =
    Mesh.Mesh


{-| Raw Skin data
-}
type alias Skin =
    Skin.Skin


{-| Json Decoder
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


{-| Bytes Decoder
-}
bytesDecoder : Bytes.Decode.Decoder Gltf
bytesDecoder =
    Glb.decoder
        |> Bytes.Decode.andThen
            (\{ jsonString, buffers } ->
                case JD.decodeString (decoderWithSingleBuffer buffers) jsonString of
                    Ok gltf ->
                        Bytes.Decode.succeed gltf

                    Err _ ->
                        Bytes.Decode.fail
            )


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


{-| Get contents of a file of type .glb
-}
getBinary : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getBinary url msg =
    Http.get
        { url = url
        , expect = Http.expectBytes msg bytesDecoder
        }


{-| Get contents of a file of type .gltf
-}
getEmbedded : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getEmbedded url msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg decoder
        }
