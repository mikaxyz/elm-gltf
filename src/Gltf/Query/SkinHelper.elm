module Gltf.Query.SkinHelper exposing (skinAtIndex)

import Array
import Bytes
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Extra
import Common
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.Buffer exposing (Buffer(..))
import Gltf.Query.BufferStore exposing (BufferStore)
import Gltf.Query.Skeleton exposing (Skeleton(..))
import Gltf.Skin exposing (Skin(..))
import Gltf.Transform exposing (Transform)
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.BufferView exposing (BufferView)
import Internal.Gltf exposing (Gltf)
import Internal.Node as Node
import Internal.Skin as GltfSkin
import Math.Matrix4 as Mat4 exposing (Mat4)
import Quaternion
import Tree


skinAtIndex : Gltf -> BufferStore -> Gltf.Skin.Index -> Maybe Skin
skinAtIndex gltf bufferStore (Gltf.Skin.Index index) =
    gltf.skins
        |> Array.get index
        |> Maybe.andThen
            (\skin ->
                let
                    joints : List NodeIndex
                    joints =
                        skin.joints |> List.map (\(GltfSkin.JointNodeIndex i) -> NodeIndex i)

                    skeleton : Maybe Skeleton
                    skeleton =
                        joints
                            |> List.head
                            |> Maybe.andThen
                                (\(NodeIndex rootNode) ->
                                    skeletonFromGltf rootNode gltf
                                )

                    inverseBindMatrices : Maybe (List Mat4)
                    inverseBindMatrices =
                        skin.inverseBindMatrices
                            |> Maybe.andThen
                                (\x ->
                                    Common.accessorAtIndex gltf x
                                        |> Maybe.andThen (Common.bufferInfo gltf bufferStore)
                                        |> Maybe.map inverseBindMatricesFromBuffer
                                )
                in
                Maybe.map2
                    (\skeleton_ inverseBindMatrices_ ->
                        Skin
                            { inverseBindMatrices = inverseBindMatrices_
                            , joints = joints
                            , skeleton = skeleton_
                            , index = Gltf.Skin.Index index
                            }
                    )
                    skeleton
                    inverseBindMatrices
            )


skeletonFromGltf : Int -> Gltf -> Maybe Skeleton
skeletonFromGltf index gltf =
    let
        nodeParent : Node.Node -> Maybe Node.Node
        nodeParent (Node.Node child) =
            gltf.nodes
                |> Array.toList
                |> List.filter (\(Node.Node node) -> node.children |> List.member child.index)
                |> List.head

        transformToMat4 : Transform -> Mat4
        transformToMat4 transform =
            case transform of
                Gltf.Transform.TRS { translation, rotation, scale } ->
                    let
                        t : Mat4
                        t =
                            translation |> Maybe.map Mat4.makeTranslate |> Maybe.withDefault Mat4.identity

                        r : Mat4
                        r =
                            rotation |> Maybe.map Quaternion.toMat4 |> Maybe.withDefault Mat4.identity

                        s : Mat4
                        s =
                            scale |> Maybe.map Mat4.makeScale |> Maybe.withDefault Mat4.identity
                    in
                    s
                        |> Mat4.mul r
                        |> Mat4.mul t

                Gltf.Transform.Matrix _ ->
                    -- TODO: Why do we need to ignore this? (Example: BrainStem)
                    Mat4.identity

        nodeParentTransforms : Node.Node -> List Mat4 -> Mat4
        nodeParentTransforms child transforms =
            case nodeParent child of
                Just (Node.Node parent) ->
                    nodeParentTransforms (Node.Node parent) (transformToMat4 parent.transform :: transforms)

                Nothing ->
                    transforms |> List.reverse |> List.foldl Mat4.mul Mat4.identity
    in
    Common.maybeNodeTree gltf (Node.Index index)
        |> Maybe.map
            (\nodes ->
                let
                    skeletonRoot : Node.Node
                    skeletonRoot =
                        Tree.label nodes

                    nodeParentTransform : Mat4
                    nodeParentTransform =
                        nodeParentTransforms skeletonRoot []
                in
                nodes
                    |> Tree.map
                        (\(Node.Node node) ->
                            { nodeIndex = node.index |> (\(Node.Index i) -> NodeIndex i)
                            , transform = node.transform
                            }
                        )
                    |> Skeleton nodeParentTransform
            )


inverseBindMatricesFromBuffer : ( Accessor, BufferView, Buffer ) -> List Mat4
inverseBindMatricesFromBuffer ( accessor, bufferView, Buffer buffer ) =
    buffer
        |> Bytes.Extra.drop (accessor.byteOffset + bufferView.byteOffset)
        |> Bytes.Extra.take bufferView.byteLength
        |> Bytes.Decode.decode
            (Bytes.Decode.Extra.list accessor.count (mat4Decoder accessor bufferView))
        |> Maybe.withDefault []


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
