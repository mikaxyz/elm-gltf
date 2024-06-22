module Internal.Gltf exposing
    ( Asset
    , Gltf
    , bytesDecoder
    , decoder
    )

import Array exposing (Array)
import Bytes.Decode
import Gltf.Glb as Glb
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.Animation as Animation exposing (Animation)
import Internal.Buffer as Buffer exposing (Buffer)
import Internal.BufferView as BufferView exposing (BufferView)
import Internal.Camera as Camera exposing (Camera)
import Internal.Image as Image exposing (Image)
import Internal.Material as Material exposing (Material)
import Internal.Mesh as Mesh exposing (Mesh)
import Internal.Node as Node exposing (Node)
import Internal.Sampler as Sampler exposing (Sampler)
import Internal.Scene as Scene exposing (Scene)
import Internal.Skin as Skin exposing (Skin)
import Internal.Texture as Texture exposing (Texture)
import Internal.Util as Util
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type alias Gltf =
    { asset : Asset
    , scenes : Array Scene
    , nodes : Array Node
    , meshes : Array Mesh
    , images : Array Image
    , materials : Array Material
    , samplers : Array Sampler
    , textures : Array Texture
    , skins : Array Skin
    , buffers : Array Buffer
    , bufferViews : Array BufferView
    , accessors : Array Accessor
    , animations : Array Animation
    , cameras : Array Camera
    }


type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


decoder : JD.Decoder Gltf
decoder =
    JD.succeed Gltf
        |> JDP.required "asset" assetDecoder
        |> JDP.required "scenes" (JD.array Scene.decoder)
        |> JDP.required "nodes" (Util.arrayWithIndexedItems Node.decoder)
        |> JDP.required "meshes" (JD.array Mesh.decoder)
        |> JDP.optional "images" (JD.array Image.decoder) Array.empty
        |> JDP.optional "materials" (JD.array Material.decoder) Array.empty
        |> JDP.optional "samplers" (JD.array Sampler.decoder) Array.empty
        |> JDP.optional "textures" (JD.array Texture.decoder) Array.empty
        |> JDP.optional "skins" (JD.array Skin.decoder) Array.empty
        |> JDP.required "buffers" (JD.array Buffer.decoder)
        |> JDP.required "bufferViews" (JD.array BufferView.decoder)
        |> JDP.required "accessors" (JD.array Accessor.decoder)
        |> JDP.optional "animations" (JD.array Animation.decoder) Array.empty
        |> JDP.optional "cameras" (Util.arrayWithIndexedItems Camera.decoder) Array.empty


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
        |> JDP.optional "images" (JD.array Image.decoder) Array.empty
        |> JDP.optional "materials" (JD.array Material.decoder) Array.empty
        |> JDP.optional "samplers" (JD.array Sampler.decoder) Array.empty
        |> JDP.optional "textures" (JD.array Texture.decoder) Array.empty
        |> JDP.optional "skins" (JD.array Skin.decoder) Array.empty
        |> JDP.hardcoded ([ buffer ] |> Array.fromList)
        |> JDP.required "bufferViews" (JD.array BufferView.decoder)
        |> JDP.required "accessors" (JD.array Accessor.decoder)
        |> JDP.optional "animations" (JD.array Animation.decoder) Array.empty
        |> JDP.optional "cameras" (Util.arrayWithIndexedItems Camera.decoder) Array.empty


assetDecoder : JD.Decoder Asset
assetDecoder =
    JD.map3 Asset
        (JD.field "version" JD.string)
        (JD.field "copyright" JD.string |> JD.maybe)
        (JD.field "generator" JD.string |> JD.maybe)
