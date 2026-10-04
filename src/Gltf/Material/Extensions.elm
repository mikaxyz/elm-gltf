module Gltf.Material.Extensions exposing (Extensions, Dispersion(..), Ior(..), Transmission, Volume)

{-| Material extensions as defined in the [glTF specification](https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Khronos).

Currently supports:

  - KHR\_materials\_dispersion
  - KHR\_materials\_ior
  - KHR\_materials\_transmission
  - KHR\_materials\_volume

The raw JSON value is there for everything else.

@docs Extensions, Dispersion, Ior, Transmission, Volume

-}

import Gltf.Texture exposing (Texture)
import Json.Decode
import Math.Vector3 exposing (Vec3)


{-| Material extensions
-}
type alias Extensions =
    { dispersion : Maybe Dispersion
    , ior : Maybe Ior
    , transmission : Maybe Transmission
    , volume : Maybe Volume
    , raw : Json.Decode.Value
    }


{-| [KHR\_materials\_ior](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_ior/README.md) extension
-}
type Ior
    = Ior Float


{-| [KHR\_materials\_dispersion](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_dispersion/README.md) extension
-}
type Dispersion
    = Dispersion Float


{-| [KHR\_materials\_transmission](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_transmission/README.md) extension
-}
type alias Transmission =
    { factor : Float
    , texture : Maybe Texture
    }


{-| [KHR\_materials\_volume](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_volume/README.md) extension
-}
type alias Volume =
    { attenuationColor : Vec3
    , attenuationDistance : Maybe Float
    , thicknessFactor : Float
    , thicknessTexture : Maybe Texture
    }
