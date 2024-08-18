module Gltf.Material.Extensions exposing (TextureExtensions, TextureTransformExtension)

{-| Extensions from [glTF Texture Info](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-textureinfo)

Currently only supports KHR\_texture\_transform. The raw JSON value is there for everything else.

@docs TextureExtensions, TextureTransformExtension

-}

import Json.Decode
import Math.Vector2 exposing (Vec2)


{-| Texture extensions
-}
type alias TextureExtensions =
    { textureTransform : Maybe TextureTransformExtension
    , raw : Json.Decode.Value
    }


{-| [KHR\_texture\_transform](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_texture_transform/README.md) extension
-}
type alias TextureTransformExtension =
    { offset : Vec2
    , rotation : Float
    , scale : Vec2
    , texCoord : Int
    }
