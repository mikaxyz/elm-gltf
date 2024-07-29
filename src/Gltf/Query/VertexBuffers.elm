module Gltf.Query.VertexBuffers exposing
    ( VertexBuffers
    , fromPrimitive
    )

import Common
import Gltf.Query.Buffer exposing (Buffer)
import Gltf.Query.BufferStore exposing (BufferStore)
import Internal.Accessor exposing (Accessor)
import Internal.BufferView exposing (BufferView)
import Internal.Gltf exposing (Gltf)
import Internal.Mesh as Mesh exposing (Primitive)


type alias VertexBuffers =
    { position : Maybe ( Accessor, BufferView, Buffer )
    , normal : Maybe ( Accessor, BufferView, Buffer )
    , tangent : Maybe ( Accessor, BufferView, Buffer )
    , color : Maybe ( Accessor, BufferView, Buffer )
    , joints : Maybe ( Accessor, BufferView, Buffer )
    , weights : Maybe ( Accessor, BufferView, Buffer )
    , texCoords : Maybe ( Accessor, BufferView, Buffer )
    }


fromPrimitive : Gltf -> BufferStore -> Primitive -> VertexBuffers
fromPrimitive gltf bufferStore { attributes } =
    let
        bufferInfo : Accessor -> Maybe ( Accessor, BufferView, Buffer )
        bufferInfo accessor =
            Common.bufferInfo gltf bufferStore accessor
    in
    attributes
        |> List.foldl
            (\attribute acc ->
                case attribute of
                    Mesh.Position accessorIndex ->
                        { acc
                            | position =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Normal accessorIndex ->
                        { acc
                            | normal =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Tangent accessorIndex ->
                        { acc
                            | tangent =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Color _ accessorIndex ->
                        { acc
                            | color =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Joints _ accessorIndex ->
                        { acc
                            | joints =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Weights _ accessorIndex ->
                        { acc
                            | weights =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.TexCoord _ accessorIndex ->
                        { acc
                            | texCoords =
                                Common.accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Unknown _ ->
                        acc
            )
            { position = Nothing
            , normal = Nothing
            , tangent = Nothing
            , color = Nothing
            , joints = Nothing
            , weights = Nothing
            , texCoords = Nothing
            }
