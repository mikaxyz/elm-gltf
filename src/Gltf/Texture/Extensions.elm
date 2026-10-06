module Gltf.Texture.Extensions exposing (Extensions, Transform)

{-| Texture extensions as defined in the [glTF specification](https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Khronos).

Currently only supports KHR\_texture\_transform. The raw JSON value is there for everything else.

@docs Extensions, Transform

-}

import Json.Decode
import Math.Vector2 exposing (Vec2)


{-| Texture extensions
-}
type alias Extensions =
    { transform : Maybe Transform
    , raw : Json.Decode.Value
    }


{-| [KHR\_texture\_transform](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_texture_transform/README.md) extension
-}
type alias Transform =
    { offset : Vec2
    , rotation : Float
    , scale : Vec2
    , texCoord : Int
    }
