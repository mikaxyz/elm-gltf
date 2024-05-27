module Xyz.Gltf.Query.Animation exposing
    ( ExtractedAnimation(..)
    , ExtractedChannel(..)
    , ExtractedSampler(..)
    , attributeToFloat
    , attributeToQuaternion
    , attributeToVec3
    , extractAnimationWithName
    , extractAnimations
    )

import Array exposing (Array)
import Bytes
import Bytes.Decode
import Bytes.Decode.Extra
import Bytes.Extra
import Gltf exposing (Gltf)
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion
import Tree exposing (Tree)
import Xyz.Gltf.Accessor as Accessor exposing (Accessor)
import Xyz.Gltf.Animation exposing (Animation(..))
import Xyz.Gltf.Animation.Channel as Channel exposing (Channel(..))
import Xyz.Gltf.Animation.Sampler as Sampler exposing (Sampler(..))
import Xyz.Gltf.Buffer as Buffer exposing (Buffer(..))
import Xyz.Gltf.BufferView as BufferView exposing (BufferView)
import Xyz.Gltf.Mesh as Mesh exposing (Attribute, Primitive)
import Xyz.Gltf.Node as Node exposing (Node(..))


type ExtractedAnimation
    = ExtractedAnimation
        { samplers : Array ExtractedSampler
        , channels : List ExtractedChannel
        }


type ExtractedChannel
    = ExtractedChannel
        { sampler : ExtractedSampler
        , nodeIndex : Node.Index
        , path : Channel.Path
        }


type ExtractedSampler
    = ExtractedSampler
        { input : List Attribute
        , output : List Attribute
        , interpolation : Sampler.Interpolation
        }


extractAnimations : Gltf -> List ExtractedAnimation
extractAnimations gltf =
    gltf.animations
        |> Array.toList
        |> List.map (extractAnimation gltf)


extractAnimationWithName : String -> Gltf -> List ExtractedAnimation
extractAnimationWithName name gltf =
    gltf.animations
        |> Array.filter (\(Animation animation) -> animation.name == Just name)
        |> Array.toList
        |> List.map (extractAnimation gltf)


attributeToVec3 : Attribute -> Maybe Vec3
attributeToVec3 a =
    case a of
        Vec3Attribute v ->
            Vec3.fromRecord v |> Just

        _ ->
            Nothing


attributeToFloat : Attribute -> Maybe Float
attributeToFloat a =
    case a of
        ScalarFloatAttribute v ->
            Just v

        _ ->
            Nothing


attributeToQuaternion : Attribute -> Maybe Quaternion.Quaternion
attributeToQuaternion a =
    case a of
        Vec4FloatAttribute { x, y, z, w } ->
            Just (Quaternion.quaternion w x y z)

        _ ->
            Nothing


type Attribute
    = ScalarFloatAttribute Float
    | ScalarIntAttribute Int
    | Vec2Attribute { x : Float, y : Float }
    | Vec3Attribute { x : Float, y : Float, z : Float }
    | Vec4FloatAttribute { x : Float, y : Float, z : Float, w : Float }
    | Vec4IntAttribute { x : Int, y : Int, z : Int, w : Int }


nodeAtIndex : Gltf -> Node.Index -> Maybe Node
nodeAtIndex gltf (Node.Index index) =
    gltf.nodes |> Array.get index


meshAtIndex : Gltf -> Mesh.Index -> Maybe Mesh.Mesh
meshAtIndex gltf (Mesh.Index index) =
    gltf.meshes |> Array.get index


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


bufferViewAtIndex : Gltf -> BufferView.Index -> Maybe BufferView
bufferViewAtIndex gltf (BufferView.Index index) =
    gltf.bufferViews |> Array.get index


bufferAtIndex : Gltf -> Buffer.Index -> Maybe Buffer
bufferAtIndex gltf (Buffer.Index index) =
    gltf.buffers |> Array.get index


extractChannel : Array ExtractedSampler -> Channel -> Maybe ExtractedChannel
extractChannel samplers (Channel channel) =
    let
        (Sampler.Index samplerIndex) =
            channel.sampler
    in
    samplers
        |> Array.get samplerIndex
        |> Maybe.map
            (\sampler ->
                ExtractedChannel
                    { sampler = sampler
                    , nodeIndex = channel.target.node
                    , path = channel.target.path
                    }
            )


extractSampler : Gltf -> Sampler -> Maybe ExtractedSampler
extractSampler gltf (Sampler sampler) =
    Maybe.map2
        (\input output ->
            ExtractedSampler
                { input = input
                , output = output
                , interpolation = sampler.interpolation
                }
        )
        (sampler.input
            |> accessorAtIndex gltf
            |> Maybe.andThen
                (\accessor ->
                    bufferViewAtIndex gltf accessor.bufferView
                        |> Maybe.map (Tuple.pair accessor)
                )
            |> Maybe.andThen
                (\( accessor, bufferView ) ->
                    bufferAtIndex gltf bufferView.buffer
                        |> Maybe.map (\buffer -> parseBuffer ( accessor, bufferView, buffer ))
                )
        )
        (sampler.output
            |> accessorAtIndex gltf
            |> Maybe.andThen
                (\accessor ->
                    bufferViewAtIndex gltf accessor.bufferView
                        |> Maybe.map (Tuple.pair accessor)
                )
            |> Maybe.andThen
                (\( accessor, bufferView ) ->
                    bufferAtIndex gltf bufferView.buffer
                        |> Maybe.map (\buffer -> parseBuffer ( accessor, bufferView, buffer ))
                )
        )


extractAnimation : Gltf -> Animation -> ExtractedAnimation
extractAnimation gltf (Animation x) =
    let
        samplers : Array ExtractedSampler
        samplers =
            x.samplers
                |> Array.toList
                |> List.filterMap (extractSampler gltf)
                |> Array.fromList
    in
    ExtractedAnimation
        { samplers = samplers
        , channels =
            x.channels
                |> Array.toList
                |> List.filterMap (extractChannel samplers)
        }


primitiveTreeFromNode : Gltf -> Node -> Tree ( Node, Primitive )
primitiveTreeFromNode gltf (Node node_) =
    node_.children
        |> List.filterMap (nodeAtIndex gltf)
        |> primitiveTreeFromNodes gltf (Node node_)


primitiveTreeFromNodes : Gltf -> Node -> List Node -> Tree ( Node, Primitive )
primitiveTreeFromNodes gltf parentNode nodes =
    let
        parentNodeFirstPrimitive : Node -> Primitive
        parentNodeFirstPrimitive (Node x) =
            x.meshIndex
                |> Maybe.andThen (meshAtIndex gltf)
                |> Maybe.andThen (.primitives >> List.head)
                |> Maybe.withDefault (Primitive [] (Just (Accessor.Index -1)))
    in
    nodes
        |> List.map (nodeToPrimitives gltf)
        |> List.map
            (\( ( node__, primitives ), children ) ->
                case primitives of
                    first :: rest ->
                        Tree.tree ( node__, first )
                            (rest
                                |> List.map (\x -> Tree.singleton ( parentNode, x ))
                                |> List.append children
                            )

                    [] ->
                        Tree.singleton ( node__, Primitive [] (Just (Accessor.Index -1)) )
            )
        |> Tree.tree ( parentNode, parentNodeFirstPrimitive parentNode )


nodeToPrimitives : Gltf -> Node -> ( ( Node, List Primitive ), List (Tree ( Node, Primitive )) )
nodeToPrimitives gltf (Node node_) =
    ( ( Node node_
      , node_.meshIndex
            |> Maybe.andThen (meshAtIndex gltf)
            |> Maybe.map .primitives
            |> Maybe.withDefault []
      )
    , node_.children
        |> List.filterMap (nodeAtIndex gltf)
        |> List.map (primitiveTreeFromNode gltf)
    )


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
