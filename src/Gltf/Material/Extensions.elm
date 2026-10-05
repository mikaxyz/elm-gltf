module Gltf.Material.Extensions exposing
    ( Extensions
    , Anisotropy, Clearcoat, Dispersion(..), EmissiveStrength(..), Ior(..), Iridescence, Sheen, Specular, Transmission, Unlit(..), Volume
    , clearcoatTexturesPackedIndex, iridescenceTexturesPackedIndex, sheenTexturesPackedIndex
    )

{-| Material extensions as defined in the [glTF specification](https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Khronos).

Currently supports:

  - KHR\_materials\_anisotropy
  - KHR\_materials\_clearcoat
  - KHR\_materials\_dispersion
  - KHR\_materials\_emissive\_strength
  - KHR\_materials\_ior
  - KHR\_materials\_iridescence
  - KHR\_materials\_sheen
  - KHR\_materials\_specular
  - KHR\_materials\_transmission
  - KHR\_materials\_unlit
  - KHR\_materials\_volume

The raw JSON value is there for everything else.

@docs Extensions
@docs Anisotropy, Clearcoat, Dispersion, EmissiveStrength, Ior, Iridescence, Sheen, Specular, Transmission, Unlit, Volume
@docs clearcoatTexturesPackedIndex, iridescenceTexturesPackedIndex, sheenTexturesPackedIndex

-}

import Gltf.Texture exposing (Texture)
import Json.Decode
import Math.Vector3 exposing (Vec3)


{-| Material extensions
-}
type alias Extensions =
    { anisotropy : Maybe Anisotropy
    , clearcoat : Maybe Clearcoat
    , dispersion : Maybe Dispersion
    , emissiveStrength : Maybe EmissiveStrength
    , ior : Maybe Ior
    , iridescence : Maybe Iridescence
    , sheen : Maybe Sheen
    , specular : Maybe Specular
    , transmission : Maybe Transmission
    , unlit : Maybe Unlit
    , volume : Maybe Volume
    , raw : Json.Decode.Value
    }


{-| [KHR\_materials\_anisotropy](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_anisotropy/README.md) extension
-}
type alias Anisotropy =
    { strength : Float
    , rotation : Float
    , texture : Maybe Texture
    }


{-| [KHR\_materials\_clearcoat](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_clearcoat/README.md) extension
-}
type alias Clearcoat =
    { factor : Float
    , texture : Maybe Texture
    , roughnessFactor : Float
    , roughnessTexture : Maybe Texture
    , normalTexture : Maybe Texture
    , normalTextureScale : Float
    }


{-| [KHR\_materials\_dispersion](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_dispersion/README.md) extension
-}
type Dispersion
    = Dispersion Float


{-| [KHR\_materials\_emissive\_strength](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_emissive_strength/README.md) extension
-}
type EmissiveStrength
    = EmissiveStrength Float


{-| [KHR\_materials\_ior](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_ior/README.md) extension
-}
type Ior
    = Ior Float


{-| [KHR\_materials\_iridescence](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_iridescence/README.md) extension
-}
type alias Iridescence =
    { factor : Float
    , texture : Maybe Texture
    , ior : Float
    , thicknessMinimum : Float
    , thicknessMaximum : Float
    , thicknessTexture : Maybe Texture
    }


{-| [KHR\_materials\_sheen](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_sheen/README.md) extension
-}
type alias Sheen =
    { colorFactor : Vec3
    , colorTexture : Maybe Texture
    , roughnessFactor : Float
    , roughnessTexture : Maybe Texture
    }


{-| [KHR\_materials\_specular](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_specular/README.md) extension
-}
type alias Specular =
    { factor : Float
    , texture : Maybe Texture
    , colorFactor : Vec3
    , colorTexture : Maybe Texture
    }


{-| [KHR\_materials\_transmission](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_transmission/README.md) extension
-}
type alias Transmission =
    { factor : Float
    , texture : Maybe Texture
    }


{-| [KHR\_materials\_unlit](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_unlit/README.md) extension
-}
type Unlit
    = Unlit


{-| [KHR\_materials\_volume](https://github.com/KhronosGroup/glTF/blob/main/extensions/2.0/Khronos/KHR_materials_volume/README.md) extension
-}
type alias Volume =
    { attenuationColor : Vec3
    , attenuationDistance : Maybe Float
    , thicknessFactor : Float
    , thicknessTexture : Maybe Texture
    }


{-| Get clearcoat texture with the assumption there is only one reference.

If your renderer is limited by max samplers (elm-webgl) you might want to use
this to only allow materials where values are "packed" into rbg-channels of
a single texture.

The clearcoat normal texture is not included: it holds full rgb normal data
and can not share channels.

-}
clearcoatTexturesPackedIndex : Maybe Clearcoat -> Result () (Maybe Gltf.Texture.Index)
clearcoatTexturesPackedIndex maybeClearcoat =
    maybeClearcoat
        |> Maybe.map (\{ texture, roughnessTexture } -> texturePackedIndex texture roughnessTexture)
        |> Maybe.withDefault (Ok Nothing)


{-| Get iridescence texture with the assumption there is only one reference.

If your renderer is limited by max samplers (elm-webgl) you might want to use
this to only allow materials where values are "packed" into rbg-channels of
a single texture.

-}
iridescenceTexturesPackedIndex : Maybe Iridescence -> Result () (Maybe Gltf.Texture.Index)
iridescenceTexturesPackedIndex maybeIridescence =
    maybeIridescence
        |> Maybe.map (\{ texture, thicknessTexture } -> texturePackedIndex texture thicknessTexture)
        |> Maybe.withDefault (Ok Nothing)


{-| Get sheen texture with the assumption there is only one reference.

If your renderer is limited by max samplers (elm-webgl) you might want to use
this to only allow materials where values are "packed" into rbg-channels of
a single texture. The sheen color uses the rgb channels and the sheen
roughness the alpha channel.

-}
sheenTexturesPackedIndex : Maybe Sheen -> Result () (Maybe Gltf.Texture.Index)
sheenTexturesPackedIndex maybeSheen =
    maybeSheen
        |> Maybe.map (\{ colorTexture, roughnessTexture } -> texturePackedIndex colorTexture roughnessTexture)
        |> Maybe.withDefault (Ok Nothing)


texturePackedIndex : Maybe Texture -> Maybe Texture -> Result () (Maybe Gltf.Texture.Index)
texturePackedIndex maybeA maybeB =
    case ( maybeA, maybeB ) of
        ( Just a, Just b ) ->
            texturePacked a b
                |> Maybe.map (Gltf.Texture.toIndex >> Just >> Ok)
                |> Maybe.withDefault (Err ())

        ( Just a, Nothing ) ->
            a |> Gltf.Texture.toIndex |> Just |> Ok

        ( Nothing, Just b ) ->
            b |> Gltf.Texture.toIndex |> Just |> Ok

        ( Nothing, Nothing ) ->
            Ok Nothing


texturePacked : Texture -> Texture -> Maybe Texture
texturePacked a b =
    if Gltf.Texture.toIndex a == Gltf.Texture.toIndex b then
        Just a

    else
        Nothing
