module Gltf.Animation exposing
    ( Animation(..), AnimatedBone(..), AnimatedProperty(..)
    , animatedProperties, animatedBoneTransforms
    , Channel(..), Sampler(..), Interpolation(..), Path(..), Attribute
    )

{-| TODO: Docs


# Types

@docs Animation, AnimatedBone, AnimatedProperty


# Get transformation of nodes/bones in a specific point of time

@docs animatedProperties, animatedBoneTransforms


# TODO: Internal, move out

@docs Channel, Sampler, Interpolation, Path, Attribute

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.Attribute as Attribute
import Gltf.Query.Skeleton as Skeleton exposing (Skeleton(..))
import Gltf.Skin exposing (Skin(..))
import Gltf.Transform as Transform
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion exposing (Quaternion)
import Tree exposing (Tree)


{-| TODO: Docs
-}
type Path
    = Translation
    | Rotation
    | Scale
    | Weights


{-| TODO: Docs
-}
type Interpolation
    = Linear
    | Step
    | CubicSpline


{-| TODO: Docs
-}
type Animation
    = Animation
        { name : Maybe String
        , samplers : Array Sampler
        , channels : List Channel
        }


{-| TODO: Docs
-}
type Channel
    = Channel
        { sampler : Sampler
        , nodeIndex : NodeIndex
        , path : Path
        }


{-| TODO: Docs
-}
type alias Attribute =
    Attribute.Attribute


{-| TODO: Docs
-}
type Sampler
    = Sampler
        { input : List Attribute
        , output : List Attribute
        , interpolation : Interpolation
        }


type Animated
    = Animated { from : Vec3, to : Vec3, w : Float, position : Vec3 }


type AnimatedRotation
    = AnimatedRotation { from : Quaternion, to : Quaternion, w : Float, current : Quaternion }


type alias TRS =
    { skinIndex : Int
    , inverseBindMatrix : Mat4
    , globalTransform : Mat4
    , rAnimated : Mat4
    , tAnimated : Mat4
    , sAnimated : Mat4
    }


{-| TODO: Docs
-}
type AnimatedProperty
    = AnimatedPositionProperty NodeIndex Vec3
    | AnimatedRotationProperty NodeIndex Quaternion


{-| TODO: Docs
-}
animatedProperties : Float -> List Animation -> List AnimatedProperty
animatedProperties theta animations =
    let
        channels : List Channel
        channels =
            animations
                |> List.concatMap (\(Animation x) -> x.channels)

        applyChannel : Channel -> Maybe AnimatedProperty
        applyChannel (Channel channel) =
            let
                (Sampler sampler) =
                    channel.sampler

                inputMax : Float
                inputMax =
                    -- TODO: Get this from BufferView
                    sampler.input
                        |> List.filterMap Attribute.toFloat
                        |> List.maximum
                        |> Maybe.withDefault 0.0

                fract : Float
                fract =
                    theta / inputMax

                duration : Float
                duration =
                    fract - (floor fract |> toFloat)

                animationTime : Float
                animationTime =
                    inputMax * duration
            in
            case channel.path of
                Translation ->
                    animatedProperty (Channel channel) animationTime
                        |> Maybe.map (\(Animated x) -> AnimatedPositionProperty channel.nodeIndex x.position)

                Rotation ->
                    animatedRotation (Channel channel) animationTime
                        |> Maybe.map (\(AnimatedRotation x) -> AnimatedRotationProperty channel.nodeIndex x.current)

                Scale ->
                    Nothing

                Weights ->
                    Nothing
    in
    channels |> List.filterMap applyChannel


animatedProperty : Channel -> Float -> Maybe Animated
animatedProperty (Channel channel) time =
    let
        (Sampler sampler) =
            channel.sampler

        data : List ( Float, Vec3 )
        data =
            List.map2
                (\input output -> ( input, output ))
                (sampler.input |> List.filterMap Attribute.toFloat)
                (sampler.output |> List.filterMap Attribute.toVec3)

        asd : { startTime : Maybe Float, endTime : Maybe Float, from : Maybe Vec3, to : Maybe Vec3 }
        asd =
            data
                |> List.foldl
                    (\( input, output ) acc ->
                        if input < time then
                            { acc | startTime = Just input, from = Just output }

                        else if acc.endTime == Nothing then
                            { acc | endTime = Just input, to = Just output }

                        else
                            acc
                    )
                    { startTime = Nothing, endTime = Nothing, from = Nothing, to = Nothing }

        properties : Maybe { w : Float, from : Vec3, to : Vec3, timeEnd : Float, timeStart : Float, position : Vec3 }
        properties =
            Maybe.map4
                (\startTime endTime from to ->
                    let
                        rTime : Float
                        rTime =
                            (time - startTime) / (endTime - startTime)

                        distance : Float
                        distance =
                            Vec3.distance to from

                        direction : Vec3
                        direction =
                            if distance > 0 then
                                Vec3.direction to from
                                    |> Vec3.scale (distance * rTime)

                            else
                                Vec3.vec3 0 0 0
                    in
                    { w = rTime
                    , from = from
                    , to = to
                    , timeEnd = endTime
                    , timeStart = startTime
                    , position = from |> Vec3.add direction
                    }
                )
                asd.startTime
                asd.endTime
                asd.from
                asd.to
    in
    properties
        |> Maybe.map (\x -> { from = x.from, to = x.to, w = x.w, position = x.position })
        |> Maybe.map Animated


animatedRotation : Channel -> Float -> Maybe AnimatedRotation
animatedRotation (Channel channel) time =
    let
        (Sampler sampler) =
            channel.sampler

        data : List ( Float, Quaternion )
        data =
            List.map2
                (\input output -> ( input, output ))
                (sampler.input |> List.filterMap Attribute.toFloat)
                (sampler.output |> List.filterMap Attribute.toQuaternion)

        asd : { startTime : Maybe Float, endTime : Maybe Float, from : Maybe Quaternion, to : Maybe Quaternion }
        asd =
            data
                |> List.foldl
                    (\( input, output ) acc ->
                        if input < time then
                            { acc | startTime = Just input, from = Just output }

                        else if acc.endTime == Nothing then
                            { acc | endTime = Just input, to = Just output }

                        else
                            acc
                    )
                    { startTime = Nothing, endTime = Nothing, from = Nothing, to = Nothing }

        properties : Maybe { w : Float, from : Quaternion, to : Quaternion, timeEnd : Float, timeStart : Float, current : Quaternion }
        properties =
            Maybe.map4
                (\startTime endTime from to ->
                    let
                        rTime : Float
                        rTime =
                            (time - startTime) / (endTime - startTime)

                        quaternionSlerp : Quaternion -> Quaternion -> Float -> Quaternion
                        quaternionSlerp q1 q2 t =
                            let
                                -- http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/
                                cosHalfTheta_ : Float
                                cosHalfTheta_ =
                                    q1.scalar * q2.scalar + q1.vector.x * q2.vector.x + q1.vector.y * q2.vector.y + q1.vector.z * q2.vector.z

                                ( cosHalfTheta, q2n ) =
                                    if cosHalfTheta_ < 0 then
                                        ( -cosHalfTheta_, Quaternion.conjugate q2 )
                                        -- According to link above...
                                        --( -cosHalfTheta_
                                        --, Quaternion.quaternion
                                        --    -q2.scalar
                                        --    -q2.vector.x
                                        --    -q2.vector.y
                                        --    q2.vector.z
                                        --)

                                    else
                                        ( cosHalfTheta_, q2 )
                            in
                            if abs cosHalfTheta >= 1.0 then
                                q1

                            else
                                let
                                    sinHalfTheta : Float
                                    sinHalfTheta =
                                        sqrt (1.0 - cosHalfTheta * cosHalfTheta)
                                in
                                if abs sinHalfTheta < 0.001 then
                                    { scalar = q1.scalar * 0.5 + q2n.scalar * 0.5
                                    , vector =
                                        { x = q1.vector.x * 0.5 + q2n.vector.x * 0.5
                                        , y = q1.vector.y * 0.5 + q2n.vector.y * 0.5
                                        , z = q1.vector.z * 0.5 + q2n.vector.z * 0.5
                                        }
                                    }

                                else
                                    let
                                        halfTheta : Float
                                        halfTheta =
                                            acos cosHalfTheta

                                        ratioA : Float
                                        ratioA =
                                            sin ((1 - t) * halfTheta) / sinHalfTheta

                                        ratioB : Float
                                        ratioB =
                                            sin (t * halfTheta) / sinHalfTheta
                                    in
                                    { scalar = q1.scalar * ratioA + q2n.scalar * ratioB
                                    , vector =
                                        { x = q1.vector.x * ratioA + q2n.vector.x * ratioB
                                        , y = q1.vector.y * ratioA + q2n.vector.y * ratioB
                                        , z = q1.vector.z * ratioA + q2n.vector.z * ratioB
                                        }
                                    }
                    in
                    { w = rTime
                    , from = from
                    , to = to
                    , timeEnd = endTime
                    , timeStart = startTime
                    , current = quaternionSlerp from to rTime
                    }
                )
                asd.startTime
                asd.endTime
                asd.from
                asd.to
    in
    properties
        |> Maybe.map (\x -> { from = x.from, to = x.to, w = x.w, current = x.current })
        |> Maybe.map AnimatedRotation


{-| TODO: Docs
-}
type AnimatedBone
    = AnimatedBone
        { skinIndex : Int
        , inverseBindMatrix : Mat4
        , joint : Mat4
        }


{-| TODO: Docs
-}
animatedBoneTransforms : Float -> List Animation -> Skin -> List AnimatedBone
animatedBoneTransforms theta animations (Skin skin) =
    let
        pathToString : Path -> String
        pathToString path =
            case path of
                Translation ->
                    "Translation"

                Rotation ->
                    "Rotation"

                Scale ->
                    "Scale"

                Weights ->
                    "Weights"

        channels : Dict ( Int, String ) Channel
        channels =
            animations
                |> List.concatMap (\(Animation x) -> x.channels)
                |> List.map
                    (\(Channel channel) ->
                        ( ( channel.nodeIndex |> (\(NodeIndex i) -> i)
                          , pathToString channel.path
                          )
                        , Channel channel
                        )
                    )
                |> Dict.fromList

        nodeChannel : Int -> String -> Maybe Channel
        nodeChannel nodeIndex path =
            Dict.get ( nodeIndex, path ) channels

        skeletonIndices : Dict Int ( Int, Mat4 )
        skeletonIndices =
            List.map2 Tuple.pair
                skin.joints
                skin.inverseBindMatrices
                |> List.indexedMap
                    (\index ( NodeIndex nodeIndex, inverseBindMatrix ) ->
                        ( nodeIndex, ( index, inverseBindMatrix ) )
                    )
                |> Dict.fromList

        boneToTrs : Skeleton.Bone -> TRS
        boneToTrs bone =
            let
                nodeIndex : Int
                nodeIndex =
                    bone.nodeIndex |> (\(NodeIndex index) -> index)

                ( skinIndex, inverseBindMatrix ) =
                    Dict.get nodeIndex skeletonIndices
                        |> Maybe.withDefault ( -1, Mat4.identity )

                { t, r, s } =
                    case bone.transform of
                        Transform.TRS { translation, rotation, scale } ->
                            { t = translation |> Maybe.map Mat4.makeTranslate |> Maybe.withDefault Mat4.identity
                            , r = rotation |> Maybe.map Quaternion.toMat4 |> Maybe.withDefault Mat4.identity
                            , s = scale |> Maybe.map Mat4.makeScale |> Maybe.withDefault Mat4.identity
                            }

                        Transform.Matrix _ ->
                            { t = Mat4.identity
                            , r = Mat4.identity
                            , s = Mat4.identity
                            }

                rAnimated : Mat4
                rAnimated =
                    nodeChannel nodeIndex "Rotation"
                        |> Maybe.map (channelMatrix theta)
                        |> Maybe.withDefault r

                tAnimated : Mat4
                tAnimated =
                    nodeChannel nodeIndex "Translation"
                        |> Maybe.map (channelMatrix theta)
                        |> Maybe.withDefault t

                sAnimated : Mat4
                sAnimated =
                    nodeChannel nodeIndex "Scale"
                        |> Maybe.map (channelMatrix theta)
                        |> Maybe.withDefault s
            in
            { skinIndex = skinIndex
            , inverseBindMatrix = inverseBindMatrix
            , globalTransform = Mat4.identity
            , rAnimated = rAnimated
            , tAnimated = tAnimated
            , sAnimated = sAnimated
            }

        trsTreeWithGlobalMatrix : Mat4 -> Tree TRS -> Tree TRS
        trsTreeWithGlobalMatrix mat tree =
            let
                trs : TRS
                trs =
                    Tree.label tree

                mat_ : Mat4
                mat_ =
                    trs.rAnimated
                        |> Mat4.mul trs.tAnimated
                        |> Mat4.mul trs.sAnimated
                        |> Mat4.mul mat
            in
            tree
                |> Tree.map (\trs_ -> { trs_ | globalTransform = mat_ })
                |> Tree.mapChildren (List.map (trsTreeWithGlobalMatrix mat_))

        trsTree : Tree TRS
        trsTree =
            skin.skeleton
                |> (\(Skeleton bones) -> bones)
                |> Tree.map boneToTrs
                |> trsTreeWithGlobalMatrix Mat4.identity
    in
    trsTree
        |> Tree.flatten
        |> List.map
            (\trs ->
                AnimatedBone
                    { skinIndex = trs.skinIndex
                    , joint = trs.globalTransform
                    , inverseBindMatrix = trs.inverseBindMatrix
                    }
            )


channelMatrix : Float -> Channel -> Mat4
channelMatrix theta (Channel channel) =
    case channel.path of
        Translation ->
            let
                (Sampler sampler) =
                    channel.sampler

                input : List Float
                input =
                    sampler.input
                        |> List.filterMap Attribute.toFloat

                inputMax : Float
                inputMax =
                    -- TODO: Get this from BufferView
                    input |> List.maximum |> Maybe.withDefault 0.0

                fract : Float
                fract =
                    theta / inputMax

                duration : Float
                duration =
                    fract - (floor fract |> toFloat)

                animationTime : Float
                animationTime =
                    inputMax * duration
            in
            animatedProperty (Channel channel) animationTime
                |> Maybe.map (\(Animated x) -> x.position)
                |> Maybe.withDefault (Vec3.vec3 0 0 0)
                |> Mat4.makeTranslate

        Rotation ->
            let
                (Sampler sampler) =
                    channel.sampler

                inputMax : Float
                inputMax =
                    -- TODO: Get this from BufferView
                    sampler.input
                        |> List.filterMap Attribute.toFloat
                        |> List.maximum
                        |> Maybe.withDefault 0.0

                fract : Float
                fract =
                    theta / inputMax

                duration : Float
                duration =
                    fract - (floor fract |> toFloat)

                animationTime : Float
                animationTime =
                    inputMax * duration
            in
            animatedRotation (Channel channel) animationTime
                |> Maybe.map (\(AnimatedRotation x) -> Quaternion.toMat4 x.current)
                |> Maybe.withDefault Mat4.identity

        Scale ->
            Mat4.identity

        Weights ->
            Mat4.identity
