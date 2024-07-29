module Gltf.Query.TextureIndex exposing
    ( TextureIndex(..)
    , toComparable
    , toImageIndex
    , toSamplerIndex
    )

import Internal.Image
import Internal.Sampler


type TextureIndex
    = TextureIndex ( Maybe Internal.Sampler.Index, Internal.Image.Index )


toComparable : TextureIndex -> ( Int, Int )
toComparable (TextureIndex ( samplerIndex, Internal.Image.Index imageIndex )) =
    ( samplerIndex |> Maybe.map (\(Internal.Sampler.Index index) -> index) |> Maybe.withDefault -1
    , imageIndex
    )


toImageIndex : TextureIndex -> Internal.Image.Index
toImageIndex (TextureIndex ( _, index )) =
    index


toSamplerIndex : TextureIndex -> Maybe Internal.Sampler.Index
toSamplerIndex (TextureIndex ( index, _ )) =
    index
