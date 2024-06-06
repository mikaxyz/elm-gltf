module Gltf.Query.VertexBuffers exposing
    ( VertexBuffers
    , fromPrimitive
    )

import Common
import Gltf exposing (Gltf)
import Internal.Accessor exposing (Accessor)
import Internal.Buffer exposing (Buffer)
import Internal.BufferView exposing (BufferView)
import Internal.Mesh as Mesh exposing (Primitive)


type alias VertexBuffers =
    { position : Maybe ( Accessor, BufferView, Buffer )
    , normal : Maybe ( Accessor, BufferView, Buffer )
    , color : Maybe ( Accessor, BufferView, Buffer )
    , joints : Maybe ( Accessor, BufferView, Buffer )
    , weights : Maybe ( Accessor, BufferView, Buffer )
    , texCoords : Maybe ( Accessor, BufferView, Buffer )
    }


fromPrimitive : Gltf -> Primitive -> VertexBuffers
fromPrimitive gltf { attributes } =
    let
        bufferInfo : Accessor -> Maybe ( Accessor, BufferView, Buffer )
        bufferInfo =
            Common.bufferInfo gltf
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
            , color = Nothing
            , joints = Nothing
            , weights = Nothing
            , texCoords = Nothing
            }
