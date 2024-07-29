module Gltf.Material exposing (Material(..), Index(..), TextureIndex, AlphaMode(..), BbrMetallicRoughness)

{-| A material as defined in the [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material).

@docs Material, Index, TextureIndex, AlphaMode, BbrMetallicRoughness

-}

import Gltf.Query.TextureIndex as TextureIndex
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| Use this to get a [textureWithIndex](Gltf#textureWithIndex)
-}
type alias TextureIndex =
    TextureIndex.TextureIndex


{-| The index of the material.
-}
type Index
    = Index Int


{-| A material assigned to a [Mesh](Gltf-Mesh#Mesh) contained in a [Node](Gltf-Node#Node). Use the properties/textures in the material to render it.
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


{-| AlphaMode of the material to convert to a [WebGL.Settings.Setting](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL-Settings))
-}
type AlphaMode
    = Opaque
    | Mask Float
    | Blend


{-| The [Material PBR Metallic Roughness](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-material-pbrmetallicroughness) of the material.
-}
type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe TextureIndex
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureIndex
    }
