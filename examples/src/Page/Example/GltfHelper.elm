module Page.Example.GltfHelper exposing
    ( boneTransformsFromAnimationName
    , boneTransformsFromAnimations
    , boneTransformsFromFirstAnimation
    , modifiers
    , modifiersFromAnimationWithName
    , modifiersFromAnimations
    , objectWithSkin
    )

import Dict exposing (Dict)
import Gltf exposing (Gltf)
import Material
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion exposing (Quaternion)
import Tree exposing (Tree)
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)
import Xyz.Gltf.Animation.Channel as Channel
import Xyz.Gltf.Node as Node exposing (Node(..))
import Xyz.Gltf.Query.Animation as Animation
    exposing
        ( ExtractedAnimation(..)
        , ExtractedChannel(..)
        , ExtractedSampler(..)
        )
import Xyz.Gltf.Query.Skin exposing (Skin(..))


objectWithSkin : Skin -> Object a b -> Object a b
objectWithSkin (Skin skin) obj =
    obj
        |> Object.withSkin
            { inverseBindMatrices = skin.inverseBindMatrices
            , joints = skin.joints |> List.map (\(Node.Index index) -> index)
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


modifiers : Float -> (Node.Index -> objectId) -> Gltf -> List (Scene.Modifier objectId Material.Name)
modifiers theta objectIdMap gltf =
    Animation.extractAnimations gltf
        |> modifiersFromAnimations theta objectIdMap


modifiersFromAnimationWithName : Float -> (Node.Index -> objectId) -> String -> Gltf -> List (Scene.Modifier objectId Material.Name)
modifiersFromAnimationWithName theta objectIdMap animationName gltf =
    Animation.extractAnimationWithName animationName gltf
        |> modifiersFromAnimations theta objectIdMap


modifiersFromAnimations : Float -> (Node.Index -> objectId) -> List ExtractedAnimation -> List (Scene.Modifier objectId Material.Name)
modifiersFromAnimations theta objectIdMap animations =
    let
        channels : List Animation.ExtractedChannel
        channels =
            animations
                |> List.concatMap (\(ExtractedAnimation x) -> x.channels)

        applyChannel : ExtractedChannel -> Object objectId Material.Name -> Object objectId Material.Name
        applyChannel (ExtractedChannel channel) obj =
            case channel.path of
                Channel.Translation ->
                    let
                        (ExtractedSampler sampler) =
                            channel.sampler

                        input : List Float
                        input =
                            sampler.input
                                |> List.filterMap Animation.attributeToFloat

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

                        animated : Maybe Animated
                        animated =
                            animatedProperty (ExtractedChannel channel) animationTime

                        position : Vec3
                        position =
                            animated
                                |> Maybe.map (\(Animated x) -> x.position)
                                |> Maybe.withDefault (Object.position obj)
                    in
                    obj
                        |> Object.withPosition position

                Channel.Rotation ->
                    let
                        (ExtractedSampler sampler) =
                            channel.sampler

                        inputMax : Float
                        inputMax =
                            -- TODO: Get this from BufferView
                            sampler.input
                                |> List.filterMap Animation.attributeToFloat
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

                        animated : Maybe AnimatedRotation
                        animated =
                            animatedRotation (ExtractedChannel channel) animationTime

                        rotation : Mat4
                        rotation =
                            animated
                                |> Maybe.map (\(AnimatedRotation x) -> Quaternion.toMat4 x.current)
                                |> Maybe.withDefault (Object.rotation obj)
                    in
                    obj |> Object.withRotation rotation

                Channel.Scale ->
                    obj

                Channel.Weights ->
                    obj
    in
    channels
        |> List.map
            (\(Animation.ExtractedChannel channel) ->
                Scene.ObjectModifier (objectIdMap channel.nodeIndex)
                    (applyChannel (Animation.ExtractedChannel channel))
            )


animatedProperty : ExtractedChannel -> Float -> Maybe Animated
animatedProperty (ExtractedChannel channel) time =
    let
        (ExtractedSampler sampler) =
            channel.sampler

        data : List ( Float, Vec3 )
        data =
            List.map2
                (\input output -> ( input, output ))
                (sampler.input |> List.filterMap Animation.attributeToFloat)
                (sampler.output |> List.filterMap Animation.attributeToVec3)

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

        properties =
            Maybe.map4
                (\startTime endTime from to ->
                    let
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


animatedRotation : ExtractedChannel -> Float -> Maybe AnimatedRotation
animatedRotation (ExtractedChannel channel) time =
    let
        (ExtractedSampler sampler) =
            channel.sampler

        data : List ( Float, Quaternion )
        data =
            List.map2
                (\input output -> ( input, output ))
                (sampler.input |> List.filterMap Animation.attributeToFloat)
                (sampler.output |> List.filterMap Animation.attributeToQuaternion)

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

        properties =
            Maybe.map4
                (\startTime endTime from to ->
                    let
                        rTime =
                            (time - startTime) / (endTime - startTime)

                        quaternionSlerp : Quaternion -> Quaternion -> Float -> Quaternion
                        quaternionSlerp q1 q2 t =
                            let
                                -- http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/
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

                                halfTheta =
                                    acos cosHalfTheta

                                sinHalfTheta =
                                    sqrt (1.0 - cosHalfTheta * cosHalfTheta)
                            in
                            if abs cosHalfTheta >= 1.0 then
                                q1

                            else if abs sinHalfTheta < 0.001 then
                                { scalar = q1.scalar * 0.5 + q2n.scalar * 0.5
                                , vector =
                                    { x = q1.vector.x * 0.5 + q2n.vector.x * 0.5
                                    , y = q1.vector.y * 0.5 + q2n.vector.y * 0.5
                                    , z = q1.vector.z * 0.5 + q2n.vector.z * 0.5
                                    }
                                }

                            else
                                let
                                    ratioA =
                                        sin ((1 - t) * halfTheta) / sinHalfTheta

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


channelMatrix : Float -> ExtractedChannel -> Mat4
channelMatrix theta (ExtractedChannel channel) =
    case channel.path of
        Channel.Translation ->
            let
                (ExtractedSampler sampler) =
                    channel.sampler

                input : List Float
                input =
                    sampler.input
                        |> List.filterMap Animation.attributeToFloat

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

                animated : Maybe Animated
                animated =
                    animatedProperty (ExtractedChannel channel) animationTime

                position : Vec3
                position =
                    animated
                        |> Maybe.map (\(Animated x) -> x.position)
                        |> Maybe.withDefault (Vec3.vec3 0 0 0)
            in
            Mat4.makeTranslate position

        Channel.Rotation ->
            let
                (ExtractedSampler sampler) =
                    channel.sampler

                inputMax : Float
                inputMax =
                    -- TODO: Get this from BufferView
                    sampler.input
                        |> List.filterMap Animation.attributeToFloat
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

                animated : Maybe AnimatedRotation
                animated =
                    animatedRotation (ExtractedChannel channel) animationTime

                rotation : Mat4
                rotation =
                    animated
                        |> Maybe.map (\(AnimatedRotation x) -> Quaternion.toMat4 x.current)
                        |> Maybe.withDefault Mat4.identity
            in
            rotation

        Channel.Scale ->
            Mat4.identity

        Channel.Weights ->
            Mat4.identity


boneTransformsFromFirstAnimation : Float -> Gltf -> Object.Skin -> Tree Node -> BoneTransforms
boneTransformsFromFirstAnimation theta gltf skin skeleton =
    let
        animations : List ExtractedAnimation
        animations =
            Animation.extractAnimations gltf |> List.take 1
    in
    boneTransformsFromAnimations theta animations skin skeleton


boneTransformsFromAnimationName : Float -> String -> Gltf -> Object.Skin -> Tree Node -> BoneTransforms
boneTransformsFromAnimationName theta animationName gltf skin skeleton =
    let
        animations : List ExtractedAnimation
        animations =
            Animation.extractAnimationWithName animationName gltf
    in
    boneTransformsFromAnimations theta animations skin skeleton


boneTransformsFromAnimations : Float -> List ExtractedAnimation -> Object.Skin -> Tree Node -> BoneTransforms
boneTransformsFromAnimations theta animations skin skeleton =
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

        channels : Dict ( Int, String ) ExtractedChannel
        channels =
            animations
                |> List.concatMap (\(ExtractedAnimation x) -> x.channels)
                |> List.map
                    (\(ExtractedChannel channel) ->
                        ( ( channel.nodeIndex |> (\(Node.Index i) -> i)
                          , pathToString channel.path
                          )
                        , ExtractedChannel channel
                        )
                    )
                |> Dict.fromList

        nodeChannel : Int -> String -> Maybe ExtractedChannel
        nodeChannel nodeIndex path =
            Dict.get ( nodeIndex, path ) channels

        skeletonIndices : Dict Int ( Int, Mat4 )
        skeletonIndices =
            List.map2 Tuple.pair
                skin.joints
                skin.inverseBindMatrices
                |> List.indexedMap (\index ( nodeIndex, inverseBindMatrix ) -> ( nodeIndex, ( index, inverseBindMatrix ) ))
                |> Dict.fromList

        nodeToTrs : Node -> TRS
        nodeToTrs (Node node) =
            let
                nodeIndex : Int
                nodeIndex =
                    node.index |> (\(Node.Index index) -> index)

                ( skinIndex, inverseBindMatrix ) =
                    Dict.get nodeIndex skeletonIndices
                        |> Maybe.withDefault ( -1, Mat4.identity )

                { t, r, s } =
                    { t = node.translation |> Maybe.map (Vec3.fromRecord >> Mat4.makeTranslate) |> Maybe.withDefault Mat4.identity
                    , r = node.rotation |> Maybe.map (\{ x, y, z, w } -> Quaternion.quaternion w x y z |> Quaternion.toMat4) |> Maybe.withDefault Mat4.identity
                    , s = node.scale |> Maybe.map (Vec3.fromRecord >> Mat4.makeScale) |> Maybe.withDefault Mat4.identity
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
            skeleton
                |> Tree.map nodeToTrs
                |> trsTreeWithGlobalMatrix Mat4.identity
    in
    trsTree
        |> Tree.flatten
        |> List.foldl
            (\trs acc ->
                let
                    mat : Mat4
                    mat =
                        trs.globalTransform
                in
                case trs.skinIndex of
                    0 ->
                        { acc | joint0 = mat, inverseBindMatrix0 = trs.inverseBindMatrix }

                    1 ->
                        { acc | joint1 = mat, inverseBindMatrix1 = trs.inverseBindMatrix }

                    2 ->
                        { acc | joint2 = mat, inverseBindMatrix2 = trs.inverseBindMatrix }

                    3 ->
                        { acc | joint3 = mat, inverseBindMatrix3 = trs.inverseBindMatrix }

                    4 ->
                        { acc | joint4 = mat, inverseBindMatrix4 = trs.inverseBindMatrix }

                    5 ->
                        { acc | joint5 = mat, inverseBindMatrix5 = trs.inverseBindMatrix }

                    6 ->
                        { acc | joint6 = mat, inverseBindMatrix6 = trs.inverseBindMatrix }

                    7 ->
                        { acc | joint7 = mat, inverseBindMatrix7 = trs.inverseBindMatrix }

                    8 ->
                        { acc | joint8 = mat, inverseBindMatrix8 = trs.inverseBindMatrix }

                    9 ->
                        { acc | joint9 = mat, inverseBindMatrix9 = trs.inverseBindMatrix }

                    10 ->
                        { acc | joint10 = mat, inverseBindMatrix10 = trs.inverseBindMatrix }

                    11 ->
                        { acc | joint11 = mat, inverseBindMatrix11 = trs.inverseBindMatrix }

                    12 ->
                        { acc | joint12 = mat, inverseBindMatrix12 = trs.inverseBindMatrix }

                    13 ->
                        { acc | joint13 = mat, inverseBindMatrix13 = trs.inverseBindMatrix }

                    14 ->
                        { acc | joint14 = mat, inverseBindMatrix14 = trs.inverseBindMatrix }

                    15 ->
                        { acc | joint15 = mat, inverseBindMatrix15 = trs.inverseBindMatrix }

                    16 ->
                        { acc | joint16 = mat, inverseBindMatrix16 = trs.inverseBindMatrix }

                    17 ->
                        { acc | joint17 = mat, inverseBindMatrix17 = trs.inverseBindMatrix }

                    18 ->
                        { acc | joint18 = mat, inverseBindMatrix18 = trs.inverseBindMatrix }

                    19 ->
                        { acc | joint19 = mat, inverseBindMatrix19 = trs.inverseBindMatrix }

                    20 ->
                        { acc | joint20 = mat, inverseBindMatrix20 = trs.inverseBindMatrix }

                    21 ->
                        { acc | joint21 = mat, inverseBindMatrix21 = trs.inverseBindMatrix }

                    22 ->
                        { acc | joint22 = mat, inverseBindMatrix22 = trs.inverseBindMatrix }

                    23 ->
                        { acc | joint23 = mat, inverseBindMatrix23 = trs.inverseBindMatrix }

                    24 ->
                        { acc | joint24 = mat, inverseBindMatrix24 = trs.inverseBindMatrix }

                    _ ->
                        acc
            )
            Object.boneTransformsIdentity
