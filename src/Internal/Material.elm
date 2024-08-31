module Internal.Material exposing
    ( AlphaMode(..)
    , Index(..)
    , Material
    , NormalTextureInfo
    , OcclusionTextureInfo
    , decoder
    , indexDecoder
    )

import Gltf.Material.Extensions exposing (TextureExtensions)
import Internal.Texture as Texture
import Internal.TextureInfo as TextureInfo exposing (TextureInfo)
import Internal.Util as Util
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)


type Index
    = Index Int


type alias Material =
    { name : Maybe String
    , pbrMetallicRoughness : PbrMetallicRoughness
    , normalTexture : Maybe NormalTextureInfo
    , occlusionTexture : Maybe OcclusionTextureInfo
    , emissiveTexture : Maybe TextureInfo
    , emissiveFactor : Vec3
    , alphaMode : AlphaMode
    , doubleSided : Bool
    }


type AlphaMode
    = Opaque
    | Mask Float
    | Blend


type alias NormalTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , scale : Float
    , extensions : Maybe TextureExtensions
    }


type alias OcclusionTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , strength : Float
    , extensions : Maybe TextureExtensions
    }


type alias PbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureInfo
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureInfo
    }


defaultPbrMetallicRoughness : PbrMetallicRoughness
defaultPbrMetallicRoughness =
    { baseColorFactor = vec4 1 1 1 1
    , baseColorTexture = Nothing
    , metallicFactor = 1
    , roughnessFactor = 1
    , metallicRoughnessTexture = Nothing
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Material
decoder =
    JD.succeed Material
        |> JDP.optional "name" (JD.maybe JD.string) Nothing
        |> JDP.optional "pbrMetallicRoughness" pbrMetallicRoughnessDecoder defaultPbrMetallicRoughness
        |> JDP.optional "normalTexture" (JD.maybe normalTextureInfoDecoder) Nothing
        |> JDP.optional "occlusionTexture" (JD.maybe occlusionTextureInfoDecoder) Nothing
        |> JDP.optional "emissiveTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "emissiveFactor" Util.vec3Decoder (vec3 0 0 0)
        |> JDP.custom alphaModeDecoder
        |> JDP.optional "doubleSided" JD.bool False


normalTextureInfoDecoder : JD.Decoder NormalTextureInfo
normalTextureInfoDecoder =
    JD.succeed NormalTextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
        |> JDP.optional "scale" JD.float 1
        |> JDP.optional "extensions" (JD.maybe TextureInfo.textureExtensionsDecoder) Nothing


occlusionTextureInfoDecoder : JD.Decoder OcclusionTextureInfo
occlusionTextureInfoDecoder =
    JD.succeed OcclusionTextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
        |> JDP.optional "strength" JD.float 1
        |> JDP.optional "extensions" (JD.maybe TextureInfo.textureExtensionsDecoder) Nothing


alphaModeDecoder : JD.Decoder AlphaMode
alphaModeDecoder =
    JD.succeed
        (\alphaMode alphaCutoff ->
            case alphaMode of
                "MASK" ->
                    Mask alphaCutoff

                "BLEND" ->
                    Blend

                _ ->
                    Opaque
        )
        |> JDP.optional "alphaMode" JD.string ""
        |> JDP.optional "alphaCutoff" JD.float 0.5


vec4Decoder : JD.Decoder Vec4
vec4Decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    [ x, y, z, w ] ->
                        JD.succeed (vec4 x y z w)

                    _ ->
                        JD.fail <| "Failed to decode Vec4 " ++ (values |> List.map String.fromFloat |> String.join ",")
            )


pbrMetallicRoughnessDecoder : JD.Decoder PbrMetallicRoughness
pbrMetallicRoughnessDecoder =
    JD.succeed PbrMetallicRoughness
        |> JDP.optional "baseColorFactor" vec4Decoder defaultPbrMetallicRoughness.baseColorFactor
        |> JDP.optional "baseColorTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "metallicFactor" JD.float defaultPbrMetallicRoughness.metallicFactor
        |> JDP.optional "roughnessFactor" JD.float defaultPbrMetallicRoughness.roughnessFactor
        |> JDP.optional "metallicRoughnessTexture" (JD.maybe TextureInfo.decoder) Nothing
