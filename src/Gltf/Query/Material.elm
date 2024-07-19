module Gltf.Query.Material exposing (Material(..), Index(..), TextureIndex, AlphaMode(..), BbrMetallicRoughness)

{-| TODO: Docs

@docs Material, Index, TextureIndex, AlphaMode, BbrMetallicRoughness

-}

import Gltf.Query.TextureIndex as TextureIndex
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| TODO: Docs
-}
type alias TextureIndex =
    TextureIndex.TextureIndex


{-| TODO: Docs
-}
type Index
    = Index Int


{-| TODO: Docs
-}
type Material
    = Material
        { name : Maybe String
        , index : Index
        , pbrMetallicRoughness : BbrMetallicRoughness
        , normalTexture : Maybe TextureIndex
        , normalTextureScale : Float
        , occlusionTexture : Maybe TextureIndex
        , occlusionTextureStrength : Float
        , emissiveTexture : Maybe TextureIndex
        , emissiveFactor : Vec3
        , doubleSided : Bool
        , alphaMode : AlphaMode
        }


{-| TODO: Docs
-}
type AlphaMode
    = Opaque
    | Mask Float
    | Blend


{-| TODO: Docs
-}
type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureIndex
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureIndex
    }
