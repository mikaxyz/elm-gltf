module Gltf.Query.ResolvedMaterial exposing
    ( Material(..)
    , Texture(..)
    , fromUnresolved
    , updateTexture
    )

import Gltf.Query.Material
import Internal.Material as Internal
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL.Texture


type Material
    = Material
        { name : Maybe String
        , index : Internal.Index
        , pbrMetallicRoughness : BbrMetallicRoughness
        , normalTexture : Maybe WebGL.Texture.Texture
        , normalTextureScale : Float
        , occlusionTexture : Maybe WebGL.Texture.Texture
        , occlusionTextureStrength : Float
        , emissiveTexture : Maybe WebGL.Texture.Texture
        , emissiveFactor : Vec3
        }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe WebGL.Texture.Texture
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe WebGL.Texture.Texture
    }


type Texture a
    = BaseColorTexture a
    | MetallicRoughnessTexture a
    | NormalTexture a
    | OcclusionTexture a
    | EmissiveTexture a


updateTexture : Texture WebGL.Texture.Texture -> Material -> Material
updateTexture texture (Material material) =
    case texture of
        BaseColorTexture x ->
            Material
                { material
                    | pbrMetallicRoughness =
                        material.pbrMetallicRoughness
                            |> (\pbrMetallicRoughness -> { pbrMetallicRoughness | baseColorTexture = Just x })
                }

        MetallicRoughnessTexture x ->
            Material
                { material
                    | pbrMetallicRoughness =
                        material.pbrMetallicRoughness
                            |> (\pbrMetallicRoughness -> { pbrMetallicRoughness | metallicRoughnessTexture = Just x })
                }

        NormalTexture x ->
            Material { material | normalTexture = Just x }

        OcclusionTexture x ->
            Material { material | occlusionTexture = Just x }

        EmissiveTexture x ->
            Material { material | emissiveTexture = Just x }


fromUnresolved : Gltf.Query.Material.Material -> Material
fromUnresolved (Gltf.Query.Material.Material material) =
    Material
        { name = material.name
        , index = material.index
        , normalTexture = Nothing
        , normalTextureScale = material.normalTextureScale
        , occlusionTexture = Nothing
        , occlusionTextureStrength = material.occlusionTextureStrength
        , emissiveTexture = Nothing
        , emissiveFactor = material.emissiveFactor
        , pbrMetallicRoughness =
            { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
            , baseColorTexture = Nothing
            , metallicFactor = material.pbrMetallicRoughness.metallicFactor
            , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
            , metallicRoughnessTexture = Nothing
            }
        }
