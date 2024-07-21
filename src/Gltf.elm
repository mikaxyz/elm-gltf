module Gltf exposing
    ( Asset, Error(..)
    , getBinary, getEmbedded
    , getBinaryWithQuery, getEmbeddedWithQuery
    , Gltf
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format


# Types

@docs Asset, Error


# Load content

@docs getBinary, getEmbedded
@docs getBinaryWithQuery, getEmbeddedWithQuery


# Internal

@docs Gltf

-}

import Gltf.Query as Query
import Http
import Internal.Gltf


{-| Type representing raw data from a .gltf/.glb file
-}
type alias Gltf =
    Internal.Gltf.Gltf


{-| Information about the file
-}
type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


{-| Get default scene from a file of type **.glb**

**Note:** If file does not specify a default scene this returns the first in list

-}
getBinary : String -> (Result Error Query.QueryResult -> msg) -> Cmd msg
getBinary url msg =
    getBinaryWithQuery url Query.defaultSceneQuery msg


{-| Get default scene from a file of type **.gltf**

**Note:** If file does not specify a default scene this returns the first in list

-}
getEmbedded : String -> (Result Error Query.QueryResult -> msg) -> Cmd msg
getEmbedded url msg =
    getEmbeddedWithQuery url Query.defaultSceneQuery msg


{-| All possible failures
-}
type Error
    = HttpError Http.Error
    | QueryError Query.Error


{-| Get content from a file of type **.glb** by supplying one of following queries:

  - [Query.defaultSceneQuery](Gltf-Query#defaultSceneQuery)
  - [Query.sceneQuery](Gltf-Query#sceneQuery)

-}
getBinaryWithQuery :
    String
    -> (Gltf -> Result Query.Error Query.QueryResult)
    -> (Result Error Query.QueryResult -> msg)
    -> Cmd msg
getBinaryWithQuery url query msg =
    Http.get
        { url = url
        , expect =
            Http.expectBytes
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen (query >> Result.mapError QueryError)
                        |> msg
                )
                Internal.Gltf.bytesDecoder
        }


{-| Get content from a file of type **.gltf** by supplying one of following queries:

  - [Query.defaultSceneQuery](Gltf-Query#defaultSceneQuery)
  - [Query.sceneQuery](Gltf-Query#sceneQuery)

-}
getEmbeddedWithQuery :
    String
    -> (Gltf -> Result Query.Error Query.QueryResult)
    -> (Result Error Query.QueryResult -> msg)
    -> Cmd msg
getEmbeddedWithQuery url query msg =
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen (query >> Result.mapError QueryError)
                        |> msg
                )
                Internal.Gltf.decoder
        }
