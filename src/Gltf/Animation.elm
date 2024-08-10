module Gltf.Animation exposing
    ( Animation, AnimatedBone(..), AnimatedProperty(..)
    , animatedProperties, animatedBoneTransforms
    , name
    )

{-| glTF supports articulated and skinned animation via key frame [animations](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#animations) of [nodes'](Gltf-Node#Node) transforms.


# Types

@docs Animation, AnimatedBone, AnimatedProperty


# Get transformation of nodes/bones in a specific point of time

@docs animatedProperties, animatedBoneTransforms


# Get name

@docs name

-}

import Dict exposing (Dict)
import Gltf.Animation.Animation exposing (Animation(..))
import Gltf.Animation.Channel as Channel exposing (Channel(..))
import Gltf.Animation.Sampler exposing (Sampler(..))
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.Attribute as Attribute
import Gltf.Query.Skeleton as Skeleton exposing (Skeleton(..))
import Gltf.Skin exposing (Skin(..))
import Gltf.Transform as Transform
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion exposing (Quaternion)
import Tree exposing (Tree)


{-| A keyframe animation
-}
type alias Animation =
    Gltf.Animation.Animation.Animation


{-| An animated bone used for skeletal/skinned animation
-}
type AnimatedBone
    = AnimatedBone
        { skinIndex : Int
        , inverseBindMatrix : Mat4
        , joint : Mat4
        }


{-| The animated [Transforms](Gltf-Transform#Transform) of a [Node](Gltf-Node#Node) separated into TRS properties.

**NOTE:** Scale and weights are not supported at this point.

-}
type AnimatedProperty
    = AnimatedPositionProperty NodeIndex Vec3
    | AnimatedRotationProperty NodeIndex Quaternion


{-| The user-defined name of the animation
-}
name : Animation -> Maybe String
name (Animation animation) =
    animation.name


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


{-| With the [Animations](Gltf-Animation#Animation) received from a [Gltf.QueryResult](Gltf#animations) this will give you [AnimatedProperties](Gltf-Animation#AnimatedProperty) to transform your nodes at a point in time.
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
                Channel.Translation ->
                    animatedProperty (Channel channel) animationTime
                        |> Maybe.map (\(Animated x) -> AnimatedPositionProperty channel.nodeIndex x.position)

                Channel.Rotation ->
                    animatedRotation (Channel channel) animationTime
                        |> Maybe.map (\(AnimatedRotation x) -> AnimatedRotationProperty channel.nodeIndex x.current)

                Channel.Scale ->
                    Nothing

                Channel.Weights ->
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
                        if input <= time then
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
                        if input <= time then
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


{-| With the [Animations](Gltf-Animation#Animation) received from a [Gltf.QueryResult](Gltf#animations) and a [Skin](Gltf-Skin#Skin) this will give you [AnimatedBones](Gltf-Animation#AnimatedBone) that can be used to deform a mesh at a point in time.
-}
animatedBoneTransforms : Float -> List Animation -> Skin -> List AnimatedBone
animatedBoneTransforms theta animations (Skin skin) =
    let
        pathToString : Channel.Path -> String
        pathToString path =
            case path of
                Channel.Translation ->
                    "Translation"

                Channel.Rotation ->
                    "Rotation"

                Channel.Scale ->
                    "Scale"

                Channel.Weights ->
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
                    trs.sAnimated
                        |> Mat4.mul trs.rAnimated
                        |> Mat4.mul trs.tAnimated
                        |> Mat4.mul mat
            in
            tree
                |> Tree.map (\trs_ -> { trs_ | globalTransform = mat_ })
                |> Tree.mapChildren (List.map (trsTreeWithGlobalMatrix mat_))

        trsTree : Tree TRS
        trsTree =
            skin.skeleton
                |> (\(Skeleton baseTransform bones) ->
                        bones
                            |> Tree.map boneToTrs
                            |> trsTreeWithGlobalMatrix baseTransform
                   )
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
        Channel.Translation ->
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

        Channel.Rotation ->
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

        Channel.Scale ->
            Mat4.identity

        Channel.Weights ->
            Mat4.identity


{-| Spherically interpolates between two quaternions, providing an interpolation between rotations with constant angle change rate.

Credit: <https://github.com/away3d/away3d-core-fp11/blob/008b1d83d3330281034b90ca7072722a9f486958/src/away3d/core/math/Quaternion.as#L112>

-}
quaternionSlerp : Quaternion -> Quaternion -> Float -> Quaternion
quaternionSlerp qa qb t =
    let
        { w1, w2_, x1, x2_, y1, y2_, z1, z2_ } =
            { w1 = qa.scalar
            , x1 = qa.vector.x
            , y1 = qa.vector.y
            , z1 = qa.vector.z
            , w2_ = qb.scalar
            , x2_ = qb.vector.x
            , y2_ = qb.vector.y
            , z2_ = qb.vector.z
            }

        dot_ : Float
        dot_ =
            w1 * w2_ + x1 * x2_ + y1 * y2_ + z1 * z2_

        { dot, w2, x2, y2, z2 } =
            if dot_ < 0 then
                { dot = -dot_, w2 = -w2_, x2 = -x2_, y2 = -y2_, z2 = -z2_ }

            else
                { dot = dot_, w2 = w2_, x2 = x2_, y2 = y2_, z2 = z2_ }
    in
    if dot < 0.95 then
        let
            -- interpolate angle linearly
            angle : Float
            angle =
                acos dot

            s : Float
            s =
                1 / sin angle

            { s1, s2 } =
                { s1 = sin (angle * (1 - t)) * s
                , s2 = sin (angle * t) * s
                }

            { w, x, y, z } =
                { w = w1 * s1 + w2 * s2
                , x = x1 * s1 + x2 * s2
                , y = y1 * s1 + y2 * s2
                , z = z1 * s1 + z2 * s2
                }
        in
        Quaternion.quaternion w x y z

    else
        let
            -- nearly identical angle, interpolate linearly
            { w, x, y, z } =
                { w = w1 + t * (w2 - w1)
                , x = x1 + t * (x2 - x1)
                , y = y1 + t * (y2 - y1)
                , z = z1 + t * (z2 - z1)
                }

            len : Float
            len =
                1.0 / sqrt (w * w + x * x + y * y + z * z)
        in
        Quaternion.quaternion (w * len) (x * len) (y * len) (z * len)
