module Gltf.Query.Skin exposing
    ( Skin(..)
    , skinAtIndex
    )

import Array
import Bytes
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Extra
import Gltf exposing (Gltf)
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.Buffer as Buffer exposing (Buffer(..))
import Internal.BufferView as BufferView exposing (BufferView)
import Internal.Node as Node
import Internal.Skin as GltfSkin
import Math.Matrix4 as Mat4 exposing (Mat4)


type Skin
    = Skin
        { inverseBindMatrices : List Mat4
        , joints : List Node.Index
        }


skinAtIndex : Gltf -> GltfSkin.Index -> Maybe Skin
skinAtIndex gltf (GltfSkin.Index index) =
    gltf.skins
        |> Array.get index
        |> Maybe.map
            (\skin ->
                let
                    inverseBindMatrices_ : Maybe (List Mat4)
                    inverseBindMatrices_ =
                        skin.inverseBindMatrices
                            |> Maybe.andThen
                                (\x ->
                                    accessorAtIndex gltf x
                                        |> Maybe.andThen (bufferInfo gltf)
                                        |> Maybe.map inverseBindMatrices
                                )
                in
                Skin
                    { inverseBindMatrices = inverseBindMatrices_ |> Maybe.withDefault []
                    , joints = skin.joints |> List.map (\(GltfSkin.JointNodeIndex i) -> Node.Index i)
                    }
            )


inverseBindMatrices : ( Accessor, BufferView, Buffer ) -> List Mat4
inverseBindMatrices ( accessor, bufferView, Buffer buffer ) =
    buffer
        |> Bytes.Extra.drop (accessor.byteOffset + bufferView.byteOffset)
        |> Bytes.Extra.take bufferView.byteLength
        |> Bytes.Decode.decode
            (Bytes.Decode.Extra.list accessor.count (mat4Decoder accessor bufferView))
        |> Maybe.withDefault []


bufferInfo : Gltf -> Accessor -> Maybe ( Accessor, BufferView, Buffer )
bufferInfo gltf accessor =
    Maybe.map2 (\bufferView buffer -> ( accessor, bufferView, buffer ))
        (bufferViewAtIndex gltf accessor.bufferView)
        (readBuffer gltf accessor)


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


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


bufferViewAtIndex : Gltf -> BufferView.Index -> Maybe BufferView
bufferViewAtIndex gltf (BufferView.Index index) =
    gltf.bufferViews |> Array.get index


bufferAtIndex : Gltf -> Buffer.Index -> Maybe Buffer
bufferAtIndex gltf (Buffer.Index index) =
    gltf.buffers |> Array.get index



--


mat4Decoder : Accessor -> BufferView -> Bytes.Decode.Decoder Mat4
mat4Decoder accessor bufferView =
    let
        width : Int
        width =
            case accessor.componentType of
                Accessor.BYTE ->
                    1

                Accessor.UNSIGNED_BYTE ->
                    1

                Accessor.SHORT ->
                    2

                Accessor.UNSIGNED_SHORT ->
                    2

                Accessor.UNSIGNED_INT ->
                    4

                Accessor.FLOAT ->
                    4

        stride : Int -> Int
        stride n =
            if bufferView.byteStride > 0 then
                bufferView.byteStride - (n * width)

            else
                0
    in
    case accessor.type_ of
        Accessor.SCALAR ->
            Bytes.Decode.fail

        Accessor.VEC2 ->
            Bytes.Decode.fail

        Accessor.VEC3 ->
            Bytes.Decode.fail

        Accessor.VEC4 ->
            Bytes.Decode.fail

        Accessor.MAT2 ->
            Bytes.Decode.fail

        Accessor.MAT3 ->
            Bytes.Decode.fail

        Accessor.MAT4 ->
            let
                vec4 : Bytes.Decode.Decoder { x : Float, y : Float, z : Float, w : Float }
                vec4 =
                    Bytes.Decode.map4
                        (\x y z w -> { x = x, y = y, z = z, w = w })
                        (Bytes.Decode.float32 Bytes.LE)
                        (Bytes.Decode.float32 Bytes.LE)
                        (Bytes.Decode.float32 Bytes.LE)
                        (Bytes.Decode.float32 Bytes.LE)
            in
            Bytes.Decode.map5
                (\m1 m2 m3 m4 _ ->
                    Mat4.fromRecord
                        { m11 = m1.x
                        , m21 = m1.y
                        , m31 = m1.z
                        , m41 = m1.w
                        , m12 = m2.x
                        , m22 = m2.y
                        , m32 = m2.z
                        , m42 = m2.w
                        , m13 = m3.x
                        , m23 = m3.y
                        , m33 = m3.z
                        , m43 = m3.w
                        , m14 = m4.x
                        , m24 = m4.y
                        , m34 = m4.z
                        , m44 = m4.w
                        }
                )
                vec4
                vec4
                vec4
                vec4
                (Bytes.Decode.bytes (stride 4))
