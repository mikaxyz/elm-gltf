module Gltf.Query.MaterialHelper exposing (fromPrimitive)

import Common
import Gltf.Material exposing (AlphaMode(..), Material(..))
import Gltf.Query.TextureIndex exposing (TextureIndex(..))
import Internal.Gltf exposing (Gltf)
import Internal.Material as Internal
import Internal.Mesh exposing (Primitive)
import Internal.Texture


fromPrimitive : Gltf -> Primitive -> Maybe Material
fromPrimitive gltf primitive =
    case Maybe.map2 Tuple.pair primitive.material (primitive.material |> Maybe.andThen (Common.materialAtIndex gltf)) of
        Just ( Internal.Index index, material ) ->
            let
                baseColorTexture : Maybe TextureIndex
                baseColorTexture =
                    material.pbrMetallicRoughness.baseColorTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)
                        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
                        |> Maybe.map TextureIndex

                normalTexture : Maybe TextureIndex
                normalTexture =
                    material.normalTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)
                        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
                        |> Maybe.map TextureIndex

                occlusionTexture : Maybe TextureIndex
                occlusionTexture =
                    material.occlusionTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)
                        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
                        |> Maybe.map TextureIndex

                emissiveTexture : Maybe TextureIndex
                emissiveTexture =
                    material.emissiveTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)
                        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
                        |> Maybe.map TextureIndex

                metallicRoughnessTexture : Maybe TextureIndex
                metallicRoughnessTexture =
                    material.pbrMetallicRoughness.metallicRoughnessTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)
                        |> Maybe.andThen (\texture -> texture.source |> Maybe.map (Tuple.pair texture.sampler))
                        |> Maybe.map TextureIndex
            in
            Material
                { name = material.name
                , index = Gltf.Material.Index index
                , normalTexture = normalTexture
                , normalTextureScale = material.normalTexture |> Maybe.map .scale |> Maybe.withDefault 1.0
                , occlusionTexture = occlusionTexture
                , occlusionTextureStrength = material.occlusionTexture |> Maybe.map .strength |> Maybe.withDefault 1.0
                , emissiveTexture = emissiveTexture
                , emissiveFactor = material.emissiveFactor
                , pbrMetallicRoughness =
                    { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
                    , baseColorTexture = baseColorTexture
                    , metallicFactor = material.pbrMetallicRoughness.metallicFactor
                    , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
                    , metallicRoughnessTexture = metallicRoughnessTexture
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


textureFromTextureInfo : Gltf -> { a | index : Internal.Texture.Index } -> Maybe Internal.Texture.Texture
textureFromTextureInfo gltf textureInfo =
    Common.textureAtIndex gltf textureInfo.index
