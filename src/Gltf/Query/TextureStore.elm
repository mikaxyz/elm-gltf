module Gltf.Query.TextureStore exposing
    ( SampledTexture(..)
    , TextureStore(..)
    , get
    , init
    , insert
    , insertLoading
    , textureWithTextureIndex
    )

import Dict exposing (Dict)
import Gltf.Query.Material
import WebGL.Texture


init : TextureStore
init =
    TextureStore Dict.empty


insert : Gltf.Query.Material.TextureIndex -> WebGL.Texture.Texture -> TextureStore -> TextureStore
insert index texture (TextureStore store) =
    TextureStore <| Dict.insert (Gltf.Query.Material.toComparable index) (SampledTexture texture) store


insertLoading : Gltf.Query.Material.TextureIndex -> TextureStore -> TextureStore
insertLoading index (TextureStore store) =
    TextureStore <| Dict.insert (Gltf.Query.Material.toComparable index) SampledTextureLoading store


get : Gltf.Query.Material.TextureIndex -> TextureStore -> Maybe SampledTexture
get index (TextureStore store) =
    Dict.get (Gltf.Query.Material.toComparable index) store


type TextureStore
    = TextureStore (Dict ( Int, Int ) SampledTexture)


type SampledTexture
    = SampledTextureLoading
    | SampledTexture WebGL.Texture.Texture


textureWithTextureIndex : Gltf.Query.Material.TextureIndex -> TextureStore -> Maybe WebGL.Texture.Texture
textureWithTextureIndex index (TextureStore store) =
    Dict.get (Gltf.Query.Material.toComparable index) store
        |> Maybe.andThen
            (\sampledTexture ->
                case sampledTexture of
                    SampledTextureLoading ->
                        Nothing

                    SampledTexture texture ->
                        Just texture
            )
