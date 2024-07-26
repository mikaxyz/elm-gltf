module Gltf.Query.Attribute exposing
    ( Attribute(..)
    , parseBuffer
    , toFloat
    , toQuaternion
    , toVec2
    , toVec3
    , toVec4
    )

import Bytes
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Extra
import Gltf.Query.Buffer exposing (Buffer(..))
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.BufferView exposing (BufferView)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Quaternion


type Attribute
    = ScalarFloatAttribute Float
    | ScalarIntAttribute Int
    | Vec2Attribute { x : Float, y : Float }
    | Vec3Attribute { x : Float, y : Float, z : Float }
    | Vec4FloatAttribute { x : Float, y : Float, z : Float, w : Float }
    | Vec4IntAttribute { x : Int, y : Int, z : Int, w : Int }


toFloat : Attribute -> Maybe Float
toFloat a =
    case a of
        ScalarFloatAttribute v ->
            Just v

        _ ->
            Nothing


toVec2 : Attribute -> Maybe Vec2
toVec2 a =
    case a of
        Vec2Attribute v ->
            Vec2.fromRecord v |> Just

        _ ->
            Nothing


toVec3 : Attribute -> Maybe Vec3
toVec3 a =
    case a of
        Vec3Attribute v ->
            Vec3.fromRecord v |> Just

        _ ->
            Nothing


toVec4 : Attribute -> Maybe Vec4
toVec4 a =
    case a of
        Vec4FloatAttribute v ->
            Just (Vec4.fromRecord v)

        _ ->
            Nothing


toQuaternion : Attribute -> Maybe Quaternion.Quaternion
toQuaternion a =
    case a of
        Vec4FloatAttribute { x, y, z, w } ->
            Just (Quaternion.quaternion w x y z)

        _ ->
            Nothing


parseBuffer : ( Accessor, BufferView, Buffer ) -> List Attribute
parseBuffer ( accessor, bufferView, Buffer buffer ) =
    let
        valuesDecoder : Bytes.Decode.Decoder Attribute
        valuesDecoder =
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
                    case accessor.componentType of
                        Accessor.FLOAT ->
                            Bytes.Decode.map2 (\x _ -> ScalarFloatAttribute x)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 1))

                        Accessor.UNSIGNED_SHORT ->
                            Bytes.Decode.map2 (\x _ -> ScalarIntAttribute x)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.bytes (stride 1))

                        Accessor.UNSIGNED_INT ->
                            Bytes.Decode.map2 (\x _ -> ScalarIntAttribute x)
                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 1))

                        _ ->
                            Bytes.Decode.fail

                Accessor.VEC2 ->
                    case accessor.componentType of
                        Accessor.FLOAT ->
                            Bytes.Decode.map3 (\x y _ -> Vec2Attribute { x = x, y = y })
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 2))

                        _ ->
                            Bytes.Decode.fail

                Accessor.VEC3 ->
                    case accessor.componentType of
                        Accessor.FLOAT ->
                            Bytes.Decode.map4 (\x y z _ -> Vec3Attribute { x = x, y = y, z = z })
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 3))

                        _ ->
                            Bytes.Decode.fail

                Accessor.VEC4 ->
                    case accessor.componentType of
                        Accessor.UNSIGNED_SHORT ->
                            Bytes.Decode.map5 (\x y z w _ -> Vec4IntAttribute { x = x, y = y, z = z, w = w })
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.bytes (stride 4))

                        Accessor.UNSIGNED_INT ->
                            Bytes.Decode.map5 (\x y z w _ -> Vec4IntAttribute { x = x, y = y, z = z, w = w })
                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                (Bytes.Decode.unsignedInt32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 4))

                        Accessor.UNSIGNED_BYTE ->
                            Bytes.Decode.map5 (\x y z w _ -> Vec4IntAttribute { x = x, y = y, z = z, w = w })
                                Bytes.Decode.unsignedInt8
                                Bytes.Decode.unsignedInt8
                                Bytes.Decode.unsignedInt8
                                Bytes.Decode.unsignedInt8
                                (Bytes.Decode.bytes (stride 4))

                        Accessor.FLOAT ->
                            Bytes.Decode.map5 (\x y z w _ -> Vec4FloatAttribute { x = x, y = y, z = z, w = w })
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.float32 Bytes.LE)
                                (Bytes.Decode.bytes (stride 4))

                        _ ->
                            Bytes.Decode.fail

                Accessor.MAT2 ->
                    Bytes.Decode.fail

                Accessor.MAT3 ->
                    Bytes.Decode.fail

                Accessor.MAT4 ->
                    Bytes.Decode.fail
    in
    buffer
        |> Bytes.Extra.drop (accessor.byteOffset + bufferView.byteOffset)
        |> Bytes.Extra.take bufferView.byteLength
        |> Bytes.Decode.decode (Bytes.Decode.Extra.list accessor.count valuesDecoder)
        |> Maybe.withDefault []
