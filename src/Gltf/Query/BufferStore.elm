module Gltf.Query.BufferStore exposing
    ( BufferStore(..)
    , get
    , getItemsToLoad
    , init
    , insert
    , isComplete
    )

import Array
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Gltf.Query.Buffer exposing (Buffer(..))
import Internal.Buffer
import Internal.Gltf


type BufferStore
    = BufferStore (Dict Int Item)


type Item
    = ItemLoading { byteLength : Int, uri : String }
    | Item Buffer


init : Internal.Gltf.Gltf -> BufferStore
init gltf =
    let
        buffers : Dict Int Item
        buffers =
            gltf.buffers
                |> Array.toIndexedList
                |> List.map
                    (\( i, x ) ->
                        case x of
                            Internal.Buffer.Buffer bytes ->
                                ( i, Item (Buffer bytes) )

                            Internal.Buffer.Remote info ->
                                ( i, ItemLoading info )
                    )
                |> Dict.fromList
    in
    BufferStore buffers


isComplete : BufferStore -> Bool
isComplete (BufferStore store) =
    store
        |> Dict.values
        |> List.all
            (\x ->
                case x of
                    ItemLoading _ ->
                        False

                    Item _ ->
                        True
            )


insert : Int -> Bytes -> BufferStore -> BufferStore
insert index bytes (BufferStore store) =
    BufferStore <| Dict.insert index (Item (Buffer bytes)) store


get : Internal.Buffer.Index -> BufferStore -> Maybe Buffer
get (Internal.Buffer.Index index) (BufferStore store) =
    Dict.get index store
        |> Maybe.andThen
            (\x ->
                case x of
                    ItemLoading _ ->
                        Nothing

                    Item item ->
                        Just item
            )


getItemsToLoad : Internal.Gltf.Gltf -> BufferStore -> List ( Int, { byteLength : Int, uri : String } )
getItemsToLoad gltf (BufferStore store) =
    store
        |> Dict.toList
        |> List.filterMap
            (\( i, x ) ->
                case x of
                    ItemLoading { byteLength, uri } ->
                        Just ( i, { byteLength = byteLength, uri = gltf.path ++ uri } )

                    Item _ ->
                        Nothing
            )
