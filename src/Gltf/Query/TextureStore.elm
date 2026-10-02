module Gltf.Query.TextureStore exposing
    ( SampledTexture(..)
    , TextureStore(..)
    , get
    , init
    , insert
    , insertLoading
    , isComplete
    , textureWithTextureIndex
    )

import Dict exposing (Dict)
import Gltf.Query.TextureIndex as TextureIndex
import Gltf.Texture
import WebGL.Texture


init : TextureStore
init =
    TextureStore Dict.empty


isComplete : TextureStore -> Bool
isComplete (TextureStore store) =
    store
        |> Dict.values
        |> List.all (\x -> x /= SampledTextureLoading)


insert : Gltf.Texture.Index -> WebGL.Texture.Texture -> TextureStore -> TextureStore
insert index texture (TextureStore store) =
    TextureStore <| Dict.insert (TextureIndex.toComparable index) (SampledTexture texture) store


insertLoading : Gltf.Texture.Index -> TextureStore -> TextureStore
insertLoading index (TextureStore store) =
    TextureStore <| Dict.insert (TextureIndex.toComparable index) SampledTextureLoading store


get : Gltf.Texture.Index -> TextureStore -> Maybe SampledTexture
get index (TextureStore store) =
    Dict.get (TextureIndex.toComparable index) store


type TextureStore
    = TextureStore (Dict ( Int, Int ) SampledTexture)


type SampledTexture
    = SampledTextureLoading
    | SampledTexture WebGL.Texture.Texture


textureWithTextureIndex : Gltf.Texture.Index -> TextureStore -> Maybe WebGL.Texture.Texture
textureWithTextureIndex index (TextureStore store) =
    Dict.get (TextureIndex.toComparable index) store
        |> Maybe.andThen
            (\sampledTexture ->
                case sampledTexture of
                    SampledTextureLoading ->
                        Nothing

                    SampledTexture texture ->
                        Just texture
            )
