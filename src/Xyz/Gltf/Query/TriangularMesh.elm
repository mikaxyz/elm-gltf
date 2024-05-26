module Xyz.Gltf.Query.TriangularMesh exposing
    ( TriangularMesh(..)
    , Vertex
    , fromPrimitive
    )

import Array
import Bytes
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Extra
import Gltf exposing (Gltf)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Xyz.Gltf.Accessor as Accessor exposing (Accessor)
import Xyz.Gltf.Buffer as Buffer exposing (Buffer(..))
import Xyz.Gltf.BufferView as BufferView exposing (BufferView)
import Xyz.Gltf.Mesh exposing (Primitive)
import Xyz.Gltf.Query.VertexBuffers as VertexBuffers exposing (VertexBuffers)


type alias Vertex =
    { position : Vec3
    , normal : Maybe Vec3
    , weights : Maybe Vec4
    , joints : Maybe Joints
    }


type alias Joints =
    { j1 : Int
    , j2 : Int
    , j3 : Int
    , j4 : Int
    }


type Attribute
    = ScalarFloatAttribute Float
    | ScalarIntAttribute Int
    | Vec2Attribute { x : Float, y : Float }
    | Vec3Attribute { x : Float, y : Float, z : Float }
    | Vec4FloatAttribute { x : Float, y : Float, z : Float, w : Float }
    | Vec4IntAttribute { x : Int, y : Int, z : Int, w : Int }
    | Mat2Attribute { a : Float, b : Float }


type TriangularMesh
    = TriangularMesh (List ( Vertex, Vertex, Vertex ))
    | IndexedTriangularMesh ( List Vertex, List ( Int, Int, Int ) )


type alias VertexAttributes =
    { position : Maybe (List Attribute)
    , normal : Maybe (List Attribute)
    , joints : Maybe (List Attribute)
    , weights : Maybe (List Attribute)
    }


fromPrimitive : Gltf -> Primitive -> TriangularMesh
fromPrimitive gltf primitive =
    let
        vertexBuffers : VertexBuffers
        vertexBuffers =
            VertexBuffers.fromPrimitive gltf primitive

        vertexAttributes : VertexAttributes
        vertexAttributes =
            { position = Maybe.map parseBuffer vertexBuffers.position
            , normal = Maybe.map parseBuffer vertexBuffers.normal
            , joints = Maybe.map parseBuffer vertexBuffers.joints
            , weights = Maybe.map parseBuffer vertexBuffers.weights
            }

        attributeToWeights : Attribute -> Maybe Vec4
        attributeToWeights a =
            case a of
                Vec4FloatAttribute v ->
                    Just (Vec4.fromRecord v)

                _ ->
                    Nothing

        attributeToJoints : Attribute -> Maybe { j1 : Int, j2 : Int, j3 : Int, j4 : Int }
        attributeToJoints a =
            case a of
                Vec4IntAttribute { x, y, z, w } ->
                    Just
                        { j1 = x
                        , j2 = y
                        , j3 = z
                        , j4 = w
                        }

                _ ->
                    Nothing

        vertexAttributesToVertices : VertexAttributes -> List Vertex
        vertexAttributesToVertices a =
            case ( a.position, a.normal, ( a.weights, a.joints ) ) of
                ( Just positions, Just normals, ( Just weights, Just joints ) ) ->
                    List.map4
                        (\position normal weights_ joints_ ->
                            { position = position
                            , normal = Just normal
                            , weights = weights_
                            , joints = joints_
                            }
                        )
                        (positions |> List.filterMap attributeToVec3)
                        (normals |> List.filterMap attributeToVec3)
                        (weights |> List.map attributeToWeights)
                        (joints |> List.map attributeToJoints)

                ( Just positions, Just normals, ( Just weights, Nothing ) ) ->
                    List.map3
                        (\position normal weights_ ->
                            { position = position
                            , normal = Just normal
                            , weights = weights_
                            , joints = Nothing
                            }
                        )
                        (positions |> List.filterMap attributeToVec3)
                        (normals |> List.filterMap attributeToVec3)
                        (weights |> List.map attributeToWeights)

                ( Just positions, Just normals, ( Nothing, Nothing ) ) ->
                    List.map2
                        (\position normal ->
                            { position = position
                            , normal = Just normal
                            , weights = Nothing
                            , joints = Nothing
                            }
                        )
                        (positions |> List.filterMap attributeToVec3)
                        (normals |> List.filterMap attributeToVec3)

                ( Just positions, Nothing, ( Just weights, Just joints ) ) ->
                    List.map3
                        (\position weights_ joints_ ->
                            { position = position
                            , normal = Nothing
                            , weights = weights_
                            , joints = joints_
                            }
                        )
                        (positions |> List.filterMap attributeToVec3)
                        (weights |> List.map attributeToWeights)
                        (joints |> List.map attributeToJoints)

                ( Just positions, Nothing, ( Nothing, Nothing ) ) ->
                    List.map
                        (\position ->
                            { position = position
                            , normal = Nothing
                            , weights = Nothing
                            , joints = Nothing
                            }
                        )
                        (positions |> List.filterMap attributeToVec3)

                _ ->
                    []
    in
    case primitive.indices of
        Just indices ->
            IndexedTriangularMesh
                ( vertexAttributesToVertices vertexAttributes
                , readTriangleIndices gltf indices
                )

        Nothing ->
            let
                vertices =
                    vertexAttributesToVertices vertexAttributes
            in
            vertices
                |> List.reverse
                |> List.foldl
                    (\vertex ( c, a ) ->
                        case c of
                            [ v2, v1 ] ->
                                ( [], ( v1, v2, vertex ) :: a )

                            _ ->
                                ( vertex :: c, a )
                    )
                    ( [], [] )
                |> Tuple.second
                |> TriangularMesh


readTriangleIndices : Gltf -> Accessor.Index -> List ( Int, Int, Int )
readTriangleIndices gltf indices =
    let
        bufferInfo : Accessor -> Maybe ( Accessor, BufferView, Buffer )
        bufferInfo accessor =
            Maybe.map2 (\bufferView buffer -> ( accessor, bufferView, buffer ))
                (bufferViewAtIndex gltf accessor.bufferView)
                (readBuffer gltf accessor)

        triangleIndices : List ( Int, Int, Int )
        triangleIndices =
            accessorAtIndex gltf indices
                |> Maybe.andThen bufferInfo
                |> Maybe.map parseBuffer
                |> Maybe.withDefault []
                |> List.filterMap
                    (\a ->
                        case a of
                            ScalarIntAttribute v ->
                                Just v

                            _ ->
                                Nothing
                    )
                |> List.foldl
                    (\val ( curr, acc ) ->
                        case curr of
                            [ y, x ] ->
                                ( [], ( x, y, val ) :: acc )

                            _ ->
                                ( val :: curr, acc )
                    )
                    ( [], [] )
                |> Tuple.second
                |> List.reverse
    in
    triangleIndices


parseBuffer : ( Accessor, BufferView, Buffer ) -> List Attribute
parseBuffer ( accessor, bufferView, Buffer buffer ) =
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

        valuesDecoder : Bytes.Decode.Decoder Attribute
        valuesDecoder =
            let
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
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
                                (Bytes.Decode.unsignedInt16 Bytes.LE)
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


bufferAtIndex : Gltf -> Buffer.Index -> Maybe Buffer
bufferAtIndex gltf (Buffer.Index index) =
    gltf.buffers |> Array.get index


bufferViewAtIndex : Gltf -> BufferView.Index -> Maybe BufferView
bufferViewAtIndex gltf (BufferView.Index index) =
    gltf.bufferViews |> Array.get index


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


attributeToVec3 : Attribute -> Maybe Vec3
attributeToVec3 a =
    case a of
        Vec3Attribute v ->
            Vec3.fromRecord v |> Just

        _ ->
            Nothing
