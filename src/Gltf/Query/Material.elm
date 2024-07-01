module Gltf.Query.Material exposing
    ( Material(..)
    , TextureIndex
    , fromPrimitive
    , toComparable
    , toImageIndex
    , toSamplerIndex
    )

import Common
import Gltf exposing (Gltf)
import Internal.Image
import Internal.Material as Internal
import Internal.Mesh exposing (Primitive)
import Internal.Sampler
import Internal.Texture
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


type TextureIndex
    = TextureIndex ( Maybe Internal.Sampler.Index, Internal.Image.Index )


toComparable : TextureIndex -> ( Int, Int )
toComparable (TextureIndex ( samplerIndex, Internal.Image.Index imageIndex )) =
    ( samplerIndex |> Maybe.map (\(Internal.Sampler.Index index) -> index) |> Maybe.withDefault -1
    , imageIndex
    )


toImageIndex : TextureIndex -> Internal.Image.Index
toImageIndex (TextureIndex ( _, index )) =
    index


toSamplerIndex : TextureIndex -> Maybe Internal.Sampler.Index
toSamplerIndex (TextureIndex ( index, _ )) =
    index


type Material
    = Material
        { name : Maybe String
        , index : Internal.Index
        , pbrMetallicRoughness : BbrMetallicRoughness
        , normalTexture : Maybe TextureIndex
        , normalTextureScale : Float
        , occlusionTexture : Maybe TextureIndex
        , occlusionTextureStrength : Float
        , emissiveTexture : Maybe TextureIndex
        , emissiveFactor : Vec3
        , doubleSided : Bool
        }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureIndex
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureIndex
    }


fromPrimitive : Gltf -> Primitive -> Maybe Material
fromPrimitive gltf primitive =
    case Maybe.map2 Tuple.pair primitive.material (primitive.material |> Maybe.andThen (Common.materialAtIndex gltf)) of
        Just ( index, material ) ->
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
                , index = index
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
                }
                |> Just

        Nothing ->
            Nothing


textureFromTextureInfo : Gltf -> { a | index : Internal.Texture.Index } -> Maybe Internal.Texture.Texture
textureFromTextureInfo gltf textureInfo =
    Common.textureAtIndex gltf textureInfo.index
