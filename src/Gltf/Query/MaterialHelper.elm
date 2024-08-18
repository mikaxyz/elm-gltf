module Gltf.Query.MaterialHelper exposing (fromPrimitive)

import Common
import Gltf.Material exposing (AlphaMode(..), Material(..))
import Gltf.Material.Extensions exposing (TextureExtensions)
import Gltf.Query.TextureIndex exposing (TextureIndex(..))
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
                }
                |> Just

        Nothing ->
            Nothing


textureFromTextureInfo :
    Gltf
    -> { a | index : Internal.Texture.Index, texCoord : Int, extensions : Maybe TextureExtensions }
    -> Maybe Gltf.Material.Texture
textureFromTextureInfo gltf textureInfo =
    Common.textureAtIndex gltf textureInfo.index
        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
        |> Maybe.map
            (\textureIndex ->
                Gltf.Material.Texture
                    { index = TextureIndex textureIndex
                    , texCoord = textureInfo.texCoord
                    , extensions = textureInfo.extensions
                    }
            )
