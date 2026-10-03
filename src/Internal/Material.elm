module Internal.Material exposing
    ( AlphaMode(..)
    , ExtensionsInfo
    , Index(..)
    , Material
    , NormalTextureInfo
    , OcclusionTextureInfo
    , TransmissionExtensionInfo
    , VolumeExtensionInfo
    , decoder
    , indexDecoder
    )

import Gltf.Material.Extensions as Extensions
import Gltf.Texture.Extensions as TextureExtensions
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
    , extensions : Maybe ExtensionsInfo
    }


type AlphaMode
    = Opaque
    | Mask Float
    | Blend


type alias NormalTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , scale : Float
    , extensions : Maybe TextureExtensions.Extensions
    }


type alias OcclusionTextureInfo =
    { index : Texture.Index
    , texCoord : Int
    , strength : Float
    , extensions : Maybe TextureExtensions.Extensions
    }


type alias PbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureInfo
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureInfo
    }


type alias ExtensionsInfo =
    { dispersion : Maybe Extensions.Dispersion
    , ior : Maybe Extensions.Ior
    , transmission : Maybe TransmissionExtensionInfo
    , volume : Maybe VolumeExtensionInfo
    , raw : JD.Value
    }


type alias TransmissionExtensionInfo =
    { transmissionFactor : Float
    , transmissionTexture : Maybe TextureInfo
    }


type alias VolumeExtensionInfo =
    { attenuationColor : Vec3
    , attenuationDistance : Maybe Float
    , thicknessFactor : Float
    , thicknessTexture : Maybe TextureInfo
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
        |> JDP.optional "extensions" (JD.maybe extensionsDecoder) Nothing


extensionsDecoder : JD.Decoder ExtensionsInfo
extensionsDecoder =
    JD.value
        |> JD.andThen
            (\raw ->
                JD.succeed ExtensionsInfo
                    |> JDP.optional "KHR_materials_dispersion" (JD.maybe dispersionDecoder) Nothing
                    |> JDP.optional "KHR_materials_ior" (JD.maybe iorDecoder) Nothing
                    |> JDP.optional "KHR_materials_transmission" (JD.maybe transmissionDecoder) Nothing
                    |> JDP.optional "KHR_materials_volume" (JD.maybe volumeDecoder) Nothing
                    |> JDP.hardcoded raw
            )


dispersionDecoder : JD.Decoder Extensions.Dispersion
dispersionDecoder =
    JD.succeed Extensions.Dispersion
        |> JDP.optional "dispersion" JD.float 0


iorDecoder : JD.Decoder Extensions.Ior
iorDecoder =
    JD.succeed Extensions.Ior
        |> JDP.optional "ior" JD.float 1.5


transmissionDecoder : JD.Decoder TransmissionExtensionInfo
transmissionDecoder =
    JD.succeed TransmissionExtensionInfo
        |> JDP.optional "transmissionFactor" JD.float 0
        |> JDP.optional "transmissionTexture" (JD.maybe TextureInfo.decoder) Nothing


volumeDecoder : JD.Decoder VolumeExtensionInfo
volumeDecoder =
    JD.succeed VolumeExtensionInfo
        |> JDP.optional "attenuationColor" Util.vec3Decoder (vec3 1 1 1)
        |> JDP.optional "attenuationDistance" (JD.maybe JD.float) Nothing
        |> JDP.optional "thicknessFactor" JD.float 0
        |> JDP.optional "thicknessTexture" (JD.maybe TextureInfo.decoder) Nothing


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
