module Internal.Material exposing
    ( AlphaMode(..)
    , AnisotropyExtensionInfo
    , ClearcoatExtensionInfo
    , ExtensionsInfo
    , Index(..)
    , IridescenceExtensionInfo
    , Material
    , NormalTextureInfo
    , OcclusionTextureInfo
    , SheenExtensionInfo
    , SpecularExtensionInfo
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
    { anisotropy : Maybe AnisotropyExtensionInfo
    , clearcoat : Maybe ClearcoatExtensionInfo
    , dispersion : Maybe Extensions.Dispersion
    , emissiveStrength : Maybe Extensions.EmissiveStrength
    , ior : Maybe Extensions.Ior
    , iridescence : Maybe IridescenceExtensionInfo
    , sheen : Maybe SheenExtensionInfo
    , specular : Maybe SpecularExtensionInfo
    , transmission : Maybe TransmissionExtensionInfo
    , unlit : Maybe Extensions.Unlit
    , volume : Maybe VolumeExtensionInfo
    , raw : JD.Value
    }


type alias AnisotropyExtensionInfo =
    { strength : Float
    , rotation : Float
    , texture : Maybe TextureInfo
    }


type alias ClearcoatExtensionInfo =
    { factor : Float
    , texture : Maybe TextureInfo
    , roughnessFactor : Float
    , roughnessTexture : Maybe TextureInfo
    , normalTexture : Maybe NormalTextureInfo
    }


type alias IridescenceExtensionInfo =
    { factor : Float
    , texture : Maybe TextureInfo
    , ior : Float
    , thicknessMinimum : Float
    , thicknessMaximum : Float
    , thicknessTexture : Maybe TextureInfo
    }


type alias SheenExtensionInfo =
    { colorFactor : Vec3
    , colorTexture : Maybe TextureInfo
    , roughnessFactor : Float
    , roughnessTexture : Maybe TextureInfo
    }


type alias SpecularExtensionInfo =
    { factor : Float
    , texture : Maybe TextureInfo
    , colorFactor : Vec3
    , colorTexture : Maybe TextureInfo
    }


type alias TransmissionExtensionInfo =
    { factor : Float
    , texture : Maybe TextureInfo
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
                    |> JDP.optional "KHR_materials_anisotropy" (JD.maybe anisotropyDecoder) Nothing
                    |> JDP.optional "KHR_materials_clearcoat" (JD.maybe clearcoatDecoder) Nothing
                    |> JDP.optional "KHR_materials_dispersion" (JD.maybe dispersionDecoder) Nothing
                    |> JDP.optional "KHR_materials_emissive_strength" (JD.maybe emissiveStrengthDecoder) Nothing
                    |> JDP.optional "KHR_materials_ior" (JD.maybe iorDecoder) Nothing
                    |> JDP.optional "KHR_materials_iridescence" (JD.maybe iridescenceDecoder) Nothing
                    |> JDP.optional "KHR_materials_sheen" (JD.maybe sheenDecoder) Nothing
                    |> JDP.optional "KHR_materials_specular" (JD.maybe specularDecoder) Nothing
                    |> JDP.optional "KHR_materials_transmission" (JD.maybe transmissionDecoder) Nothing
                    |> JDP.optional "KHR_materials_unlit" (JD.maybe unlitDecoder) Nothing
                    |> JDP.optional "KHR_materials_volume" (JD.maybe volumeDecoder) Nothing
                    |> JDP.hardcoded raw
            )


anisotropyDecoder : JD.Decoder AnisotropyExtensionInfo
anisotropyDecoder =
    JD.succeed AnisotropyExtensionInfo
        |> JDP.optional "anisotropyStrength" JD.float 0
        |> JDP.optional "anisotropyRotation" JD.float 0
        |> JDP.optional "anisotropyTexture" (JD.maybe TextureInfo.decoder) Nothing


clearcoatDecoder : JD.Decoder ClearcoatExtensionInfo
clearcoatDecoder =
    JD.succeed ClearcoatExtensionInfo
        |> JDP.optional "clearcoatFactor" JD.float 0
        |> JDP.optional "clearcoatTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "clearcoatRoughnessFactor" JD.float 0
        |> JDP.optional "clearcoatRoughnessTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "clearcoatNormalTexture" (JD.maybe normalTextureInfoDecoder) Nothing


dispersionDecoder : JD.Decoder Extensions.Dispersion
dispersionDecoder =
    JD.succeed Extensions.Dispersion
        |> JDP.optional "dispersion" JD.float 0


emissiveStrengthDecoder : JD.Decoder Extensions.EmissiveStrength
emissiveStrengthDecoder =
    JD.succeed Extensions.EmissiveStrength
        |> JDP.optional "emissiveStrength" JD.float 1


iorDecoder : JD.Decoder Extensions.Ior
iorDecoder =
    JD.succeed Extensions.Ior
        |> JDP.optional "ior" JD.float 1.5


iridescenceDecoder : JD.Decoder IridescenceExtensionInfo
iridescenceDecoder =
    JD.succeed IridescenceExtensionInfo
        |> JDP.optional "iridescenceFactor" JD.float 0
        |> JDP.optional "iridescenceTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "iridescenceIor" JD.float 1.3
        |> JDP.optional "iridescenceThicknessMinimum" JD.float 100
        |> JDP.optional "iridescenceThicknessMaximum" JD.float 400
        |> JDP.optional "iridescenceThicknessTexture" (JD.maybe TextureInfo.decoder) Nothing


sheenDecoder : JD.Decoder SheenExtensionInfo
sheenDecoder =
    JD.succeed SheenExtensionInfo
        |> JDP.optional "sheenColorFactor" Util.vec3Decoder (vec3 0 0 0)
        |> JDP.optional "sheenColorTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "sheenRoughnessFactor" JD.float 0
        |> JDP.optional "sheenRoughnessTexture" (JD.maybe TextureInfo.decoder) Nothing


specularDecoder : JD.Decoder SpecularExtensionInfo
specularDecoder =
    JD.succeed SpecularExtensionInfo
        |> JDP.optional "specularFactor" JD.float 1
        |> JDP.optional "specularTexture" (JD.maybe TextureInfo.decoder) Nothing
        |> JDP.optional "specularColorFactor" Util.vec3Decoder (vec3 1 1 1)
        |> JDP.optional "specularColorTexture" (JD.maybe TextureInfo.decoder) Nothing


unlitDecoder : JD.Decoder Extensions.Unlit
unlitDecoder =
    JD.succeed Extensions.Unlit


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
