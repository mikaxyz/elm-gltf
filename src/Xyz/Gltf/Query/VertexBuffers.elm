module Xyz.Gltf.Query.VertexBuffers exposing
    ( VertexBuffers
    , fromPrimitive
    )

import Array
import Xyz.Gltf.Accessor as Accessor exposing (Accessor)
import Xyz.Gltf.Buffer as Buffer exposing (Buffer)
import Xyz.Gltf.BufferView as BufferView exposing (BufferView)
import Xyz.Gltf.Mesh as Mesh exposing (Primitive)
import Xyz.Gltf.Raw.Gltf exposing (Gltf)


type alias VertexBuffers =
    { position : Maybe ( Accessor, BufferView, Buffer )
    , normal : Maybe ( Accessor, BufferView, Buffer )
    , joints : Maybe ( Accessor, BufferView, Buffer )
    , weights : Maybe ( Accessor, BufferView, Buffer )
    }


fromPrimitive : Gltf -> Primitive -> VertexBuffers
fromPrimitive gltf { attributes } =
    let
        bufferInfo : Accessor -> Maybe ( Accessor, BufferView, Buffer )
        bufferInfo accessor =
            Maybe.map2 (\bufferView buffer -> ( accessor, bufferView, buffer ))
                (bufferViewAtIndex gltf accessor.bufferView)
                (readBuffer gltf accessor)
    in
    attributes
        |> List.foldl
            (\attribute acc ->
                case attribute of
                    Mesh.Position accessorIndex ->
                        { acc
                            | position =
                                accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Normal accessorIndex ->
                        { acc
                            | normal =
                                accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Joints _ accessorIndex ->
                        { acc
                            | joints =
                                accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Weights _ accessorIndex ->
                        { acc
                            | weights =
                                accessorAtIndex gltf accessorIndex
                                    |> Maybe.andThen bufferInfo
                        }

                    Mesh.Unknown _ ->
                        acc
            )
            { position = Nothing
            , normal = Nothing
            , joints = Nothing
            , weights = Nothing
            }


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


bufferViewAtIndex : Gltf -> BufferView.Index -> Maybe BufferView
bufferViewAtIndex gltf (BufferView.Index index) =
    gltf.bufferViews |> Array.get index


bufferAtIndex : Gltf -> Buffer.Index -> Maybe Buffer
bufferAtIndex gltf (Buffer.Index index) =
    gltf.buffers |> Array.get index


readBuffer : Gltf -> Accessor -> Maybe Buffer
readBuffer gltf accessor =
    let
        maybeBufferView : BufferView.Index -> Maybe BufferView
        maybeBufferView x =
            bufferViewAtIndex gltf x

        maybeBuffer : BufferView -> Maybe Buffer
        maybeBuffer { buffer } =
            bufferAtIndex gltf buffer
    in
    accessor.bufferView
        |> maybeBufferView
        |> Maybe.andThen maybeBuffer
