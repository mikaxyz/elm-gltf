module Gltf.Query.ResolvedMaterial exposing
    ( Material(..)
    , fromUnresolved
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
        }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe WebGL.Texture.Texture
    , metallicFactor : Float
    , roughnessFactor : Float
    }


fromUnresolved : WebGL.Texture.Texture -> Gltf.Query.Material.Material -> Material
fromUnresolved texture (Gltf.Query.Material.Material material) =
    Material
        { name = material.name
        , index = material.index
        , pbrMetallicRoughness =
            { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
            , baseColorTexture = Just texture
            , metallicFactor = material.pbrMetallicRoughness.metallicFactor
            , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
            }
        }
