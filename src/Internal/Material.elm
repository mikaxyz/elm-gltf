module Internal.Material exposing
    ( Index(..)
    , Material
    , decoder
    , indexDecoder
    )

import Internal.Texture as Texture
import Internal.TextureInfo as TextureInfo exposing (TextureInfo)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)


type Index
    = Index Int


type alias Material =
    { name : Maybe String
    , pbrMetallicRoughness : BbrMetallicRoughness
    , normalTexture : Maybe NormalTextureInfo
    , occlusionTexture : Maybe OcclusionTextureInfo
    , emissiveTexture : Maybe TextureInfo
    , emissiveFactor : Vec3
    , alphaMode : AlphaMode
    , alphaCutoff : Float
    , doubleSided : Bool
    }


type AlphaMode
    = Opaque
    | Mask
    | Blend


type alias NormalTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , scale : Float
    }


type alias OcclusionTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , strength : Float
    }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureInfo
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureInfo
    }


defaultBbrMetallicRoughness : BbrMetallicRoughness
defaultBbrMetallicRoughness =
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
        |> JDP.optional "pbrMetallicRoughness" pbrMetallicRoughnessDecoder defaultBbrMetallicRoughness
        |> JDP.optional "normalTexture" (JD.maybe normalTextureInfoDecoder) Nothing
        |> JDP.optional "occlusionTexture" (JD.maybe occlusionTextureInfoDecoder) Nothing
        |> JDP.optional "emissiveTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "baseColorFactor" vec3Decoder (vec3 0 0 0)
        |> JDP.optional "alphaMode" alphaModeDecoder Opaque
        |> JDP.optional "alphaCutoff" JD.float 0.5
        |> JDP.optional "doubleSided" JD.bool False


normalTextureInfoDecoder : JD.Decoder NormalTextureInfo
normalTextureInfoDecoder =
    JD.succeed NormalTextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
        |> JDP.optional "scale" JD.float 1


occlusionTextureInfoDecoder : JD.Decoder OcclusionTextureInfo
occlusionTextureInfoDecoder =
    JD.succeed OcclusionTextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
        |> JDP.optional "strength" JD.float 1


alphaModeDecoder : JD.Decoder AlphaMode
alphaModeDecoder =
    JD.string
        |> JD.andThen
            (\id ->
                case id of
                    "OPAQUE" ->
                        JD.succeed Opaque

                    "MASK" ->
                        JD.succeed Mask

                    "BLEND" ->
                        JD.succeed Blend

                    _ ->
                        JD.fail ("unknown value for AlphaMode: " ++ id)
            )


vec3Decoder : JD.Decoder Vec3
vec3Decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    [ x, y, z ] ->
                        JD.succeed (vec3 x y z)

                    _ ->
                        JD.fail <| "Failed to decode Vec3 " ++ (values |> List.map String.fromFloat |> String.join ",")
            )


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


pbrMetallicRoughnessDecoder : JD.Decoder BbrMetallicRoughness
pbrMetallicRoughnessDecoder =
    JD.succeed BbrMetallicRoughness
        |> JDP.optional "baseColorFactor" vec4Decoder defaultBbrMetallicRoughness.baseColorFactor
        |> JDP.optional "baseColorTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "metallicFactor" JD.float defaultBbrMetallicRoughness.metallicFactor
        |> JDP.optional "roughnessFactor" JD.float defaultBbrMetallicRoughness.roughnessFactor
        |> JDP.optional "metallicRoughnessTexture" (JD.maybe TextureInfo.decoder) Nothing
