module Gltf.Texture exposing (Texture(..), Index, toIndex)

{-| A texture as defined in the [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-textureinfo).

@docs Texture, Index, toIndex

-}

import Gltf.Query.TextureIndex as TextureIndex
import Gltf.Texture.Extensions exposing (Extensions)


{-| Use this to get a [textureWithIndex](Gltf#textureWithIndex)
-}
type alias Index =
    TextureIndex.TextureIndex


{-| Any data associated with a texture

Texture extensions are available in [Extensions](Gltf-Texture-Extensions).

-}
type Texture
    = Texture
        { index : Index
        , texCoord : Int
        , extensions : Maybe Extensions
        }


{-| Index from Texture
-}
toIndex : Texture -> Index
toIndex (Texture texture) =
    texture.index
