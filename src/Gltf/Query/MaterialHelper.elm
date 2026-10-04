module Gltf.Query.MaterialHelper exposing (fromPrimitive)

import Common
import Gltf.Material exposing (AlphaMode(..), Material(..))
import Gltf.Material.Extensions exposing (Extensions)
import Gltf.Query.TextureIndex exposing (TextureIndex(..))
import Gltf.Texture exposing (Texture(..))
import Gltf.Texture.Extensions as TextureExtensions
import Internal.Gltf exposing (Gltf)
import Internal.Material as Internal
import Internal.Mesh exposing (Primitive)
import Internal.Texture


fromPrimitive : Gltf -> Primitive -> Maybe Material
fromPrimitive gltf primitive =
    case Maybe.map2 Tuple.pair primitive.material (primitive.material |> Maybe.andThen (Common.materialAtIndex gltf)) of
        Just ( Internal.Index index, material ) ->
            Material
                { name = material.name
                , index = Gltf.Material.Index index
                , normalTexture = material.normalTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                , normalTextureScale = material.normalTexture |> Maybe.map .scale |> Maybe.withDefault 1.0
                , occlusionTexture = material.occlusionTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                , occlusionTextureStrength = material.occlusionTexture |> Maybe.map .strength |> Maybe.withDefault 1.0
                , emissiveTexture = material.emissiveTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                , emissiveFactor = material.emissiveFactor
                , pbrMetallicRoughness =
                    { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
                    , baseColorTexture = material.pbrMetallicRoughness.baseColorTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , metallicFactor = material.pbrMetallicRoughness.metallicFactor
                    , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
                    , metallicRoughnessTexture = material.pbrMetallicRoughness.metallicRoughnessTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                , doubleSided = material.doubleSided
                , alphaMode =
                    material.alphaMode
                        |> (\alphaMode ->
                                case alphaMode of
                                    Internal.Opaque ->
                                        Opaque

                                    Internal.Mask cutoff ->
                                        Mask cutoff

                                    Internal.Blend ->
                                        Blend
                           )
                , extensions = material.extensions |> Maybe.map (extensionsFromExtensionsInfo gltf)
                }
                |> Just

        Nothing ->
            Nothing


extensionsFromExtensionsInfo : Gltf -> Internal.ExtensionsInfo -> Extensions
extensionsFromExtensionsInfo gltf extensions =
    { anisotropy =
        extensions.anisotropy
            |> Maybe.map
                (\{ strength, rotation, texture } ->
                    { strength = strength
                    , rotation = rotation
                    , texture = texture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , clearcoat =
        extensions.clearcoat
            |> Maybe.map
                (\{ factor, texture, roughnessFactor, roughnessTexture, normalTexture } ->
                    { factor = factor
                    , texture = texture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , roughnessFactor = roughnessFactor
                    , roughnessTexture = roughnessTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , normalTexture = normalTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , normalTextureScale = normalTexture |> Maybe.map .scale |> Maybe.withDefault 1.0
                    }
                )
    , dispersion = extensions.dispersion
    , emissiveStrength = extensions.emissiveStrength
    , ior = extensions.ior
    , iridescence =
        extensions.iridescence
            |> Maybe.map
                (\{ factor, texture, ior, thicknessMinimum, thicknessMaximum, thicknessTexture } ->
                    { factor = factor
                    , texture = texture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , ior = ior
                    , thicknessMinimum = thicknessMinimum
                    , thicknessMaximum = thicknessMaximum
                    , thicknessTexture = thicknessTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , sheen =
        extensions.sheen
            |> Maybe.map
                (\{ colorFactor, colorTexture, roughnessFactor, roughnessTexture } ->
                    { colorFactor = colorFactor
                    , colorTexture = colorTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , roughnessFactor = roughnessFactor
                    , roughnessTexture = roughnessTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , specular =
        extensions.specular
            |> Maybe.map
                (\{ factor, texture, colorFactor, colorTexture } ->
                    { factor = factor
                    , texture = texture |> Maybe.andThen (textureFromTextureInfo gltf)
                    , colorFactor = colorFactor
                    , colorTexture = colorTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , transmission =
        extensions.transmission
            |> Maybe.map
                (\{ factor, texture } ->
                    { factor = factor
                    , texture = texture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , unlit = extensions.unlit
    , volume =
        extensions.volume
            |> Maybe.map
                (\{ attenuationColor, attenuationDistance, thicknessFactor, thicknessTexture } ->
                    { attenuationColor = attenuationColor
                    , attenuationDistance = attenuationDistance
                    , thicknessFactor = thicknessFactor
                    , thicknessTexture = thicknessTexture |> Maybe.andThen (textureFromTextureInfo gltf)
                    }
                )
    , raw = extensions.raw
    }


textureFromTextureInfo :
    Gltf
    -> { a | index : Internal.Texture.Index, texCoord : Int, extensions : Maybe TextureExtensions.Extensions }
    -> Maybe Texture
textureFromTextureInfo gltf textureInfo =
    Common.textureAtIndex gltf textureInfo.index
        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
        |> Maybe.map
            (\textureIndex ->
                Texture
                    { index = TextureIndex textureIndex
                    , texCoord = textureInfo.texCoord
                    , extensions = textureInfo.extensions
                    }
            )
