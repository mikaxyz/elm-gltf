module Gltf.Query.ResolvedMaterial exposing
    ( Material(..)
    , Texture(..)
    , fromUnresolved
    , fromUnresolvedWithBaseColorTexture
    , updateTexture
    )

import Gltf.Query.Material
import Internal.Material as Internal
import Math.Vector4 exposing (Vec4)
import WebGL.Texture


type Material
    = Material
        { name : Maybe String
        , index : Internal.Index
        , pbrMetallicRoughness : BbrMetallicRoughness
        , normalTexture : Maybe WebGL.Texture.Texture
        }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe WebGL.Texture.Texture
    , metallicFactor : Float
    , roughnessFactor : Float
    }


type Texture
    = BaseColorTexture WebGL.Texture.Texture
    | NormalTexture WebGL.Texture.Texture


updateTexture : Texture -> Material -> Material
updateTexture texture (Material material) =
    case texture of
        BaseColorTexture x ->
            Material
                { material
                    | pbrMetallicRoughness =
                        material.pbrMetallicRoughness
                            |> (\pbrMetallicRoughness -> { pbrMetallicRoughness | baseColorTexture = Just x })
                }

        NormalTexture x ->
            Material { material | normalTexture = Just x }


fromUnresolved : Texture -> Gltf.Query.Material.Material -> Material
fromUnresolved texture material =
    case texture of
        BaseColorTexture x ->
            fromUnresolvedWithBaseColorTexture x material

        NormalTexture x ->
            fromUnresolvedWithNormalTexture x material


fromUnresolvedWithBaseColorTexture : WebGL.Texture.Texture -> Gltf.Query.Material.Material -> Material
fromUnresolvedWithBaseColorTexture texture (Gltf.Query.Material.Material material) =
    Material
        { name = material.name
        , index = material.index
        , normalTexture = Nothing
        , pbrMetallicRoughness =
            { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
            , baseColorTexture = Just texture
            , metallicFactor = material.pbrMetallicRoughness.metallicFactor
            , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
            }
        }


fromUnresolvedWithNormalTexture : WebGL.Texture.Texture -> Gltf.Query.Material.Material -> Material
fromUnresolvedWithNormalTexture texture (Gltf.Query.Material.Material material) =
    Material
        { name = material.name
        , index = material.index
        , normalTexture = Just texture
        , pbrMetallicRoughness =
            { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
            , baseColorTexture = Nothing
            , metallicFactor = material.pbrMetallicRoughness.metallicFactor
            , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
            }
        }
