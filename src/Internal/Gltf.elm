module Internal.Gltf exposing
    ( Asset
    , Gltf
    , bytesDecoder
    , decoder
    )

import Array exposing (Array)
import Bytes.Decode
import Gltf.Camera
import Gltf.Glb as Glb
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.Animation as Animation exposing (Animation)
import Internal.Buffer as Buffer exposing (Buffer)
import Internal.BufferView as BufferView exposing (BufferView)
import Internal.Camera as Camera
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
    { path : String
    , asset : Asset
    , scenes : Array Scene
    , scene : Scene.Index
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
    , cameras : Array Gltf.Camera.Camera
    }


type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


pathFromUrl : String -> String
pathFromUrl url =
    case String.split "/" url |> List.reverse |> List.drop 1 of
        [] ->
            ""

        rest ->
            (rest |> List.reverse |> String.join "/") ++ "/"


decoder : String -> JD.Decoder Gltf
decoder url =
    JD.field "buffers" (JD.array Buffer.decoder)
        |> JD.andThen (decoderWithBuffers url)


bytesDecoder : String -> Bytes.Decode.Decoder Gltf
bytesDecoder url =
    Glb.decoder
        |> Bytes.Decode.andThen
            (\{ jsonString, buffers } ->
                case JD.decodeString (decoderWithBuffers url buffers) jsonString of
                    Ok gltf ->
                        Bytes.Decode.succeed gltf

                    Err _ ->
                        Bytes.Decode.fail
            )


decoderWithBuffers : String -> Array Buffer -> JD.Decoder Gltf
decoderWithBuffers url buffers =
    JD.succeed (Gltf (pathFromUrl url))
        |> JDP.required "asset" assetDecoder
        |> JDP.required "scenes" (JD.array Scene.decoder)
        |> JDP.optional "scene" (JD.int |> JD.map Scene.Index) (Scene.Index 0)
        |> JDP.required "nodes" (Util.arrayWithIndexedItems Node.decoder)
        |> JDP.required "meshes" (JD.array Mesh.decoder)
        |> JDP.optional "images" (JD.array Image.decoder) Array.empty
        |> JDP.optional "materials" (JD.array Material.decoder) Array.empty
        |> JDP.optional "samplers" (JD.array Sampler.decoder) Array.empty
        |> JDP.optional "textures" (JD.array Texture.decoder) Array.empty
        |> JDP.optional "skins" (JD.array Skin.decoder) Array.empty
        |> JDP.hardcoded buffers
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
