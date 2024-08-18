module Internal.TextureInfo exposing
    ( Index(..)
    , TextureInfo
    , decoder
    , indexDecoder
    , textureExtensionsDecoder
    )

import Gltf.Material.Extensions exposing (TextureExtensions, TextureTransformExtension)
import Internal.Texture as Texture
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Vector2 exposing (Vec2, vec2)


type Index
    = Index Int


type alias TextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , extensions : Maybe TextureExtensions
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder TextureInfo
decoder =
    JD.succeed TextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
        |> JDP.optional "extensions" (JD.maybe textureExtensionsDecoder) Nothing


textureExtensionsDecoder : JD.Decoder TextureExtensions
textureExtensionsDecoder =
    JD.value
        |> JD.andThen
            (\raw ->
                JD.succeed TextureExtensions
                    |> JDP.optional "KHR_texture_transform" (JD.maybe textureTransformExtensionDecoder) Nothing
                    |> JDP.hardcoded raw
            )


textureTransformExtensionDecoder : JD.Decoder TextureTransformExtension
textureTransformExtensionDecoder =
    JD.succeed TextureTransformExtension
        |> JDP.optional "offset" vec2Decoder (vec2 0 0)
        |> JDP.optional "rotation" JD.float 0
        |> JDP.optional "scale" vec2Decoder (vec2 1 1)
        |> JDP.optional "texCoord" JD.int 0


vec2Decoder : JD.Decoder Vec2
vec2Decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    [ x, y ] ->
                        JD.succeed (vec2 x y)

                    _ ->
                        JD.fail <| "Could not decode Vec2 from " ++ (values |> List.map String.fromFloat |> String.join ",")
            )
