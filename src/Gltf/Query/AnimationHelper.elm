module Gltf.Query.AnimationHelper exposing
    ( extractAnimationWithName
    , extractAnimations
    )

import Array exposing (Array)
import Common
import Gltf.Query.Animation
    exposing
        ( Animation(..)
        , Channel(..)
        , Interpolation(..)
        , Path(..)
        , Sampler(..)
        )
import Gltf.Query.Attribute as Attribute
import Gltf.Query.NodeIndex exposing (NodeIndex(..))
import Internal.Accessor as Accessor
import Internal.Animation as Internal
import Internal.Animation.Channel
import Internal.Animation.Sampler
import Internal.Gltf exposing (Gltf)
import Internal.Mesh exposing (Primitive)
import Internal.Node exposing (Node(..))
import Tree exposing (Tree)


pathFromChannel : Internal.Animation.Channel.Path -> Path
pathFromChannel channelPath =
    case channelPath of
        Internal.Animation.Channel.Translation ->
            Translation

        Internal.Animation.Channel.Rotation ->
            Rotation

        Internal.Animation.Channel.Scale ->
            Scale

        Internal.Animation.Channel.Weights ->
            Weights


interpolationFromSampler : Internal.Animation.Sampler.Interpolation -> Interpolation
interpolationFromSampler interpolation =
    case interpolation of
        Internal.Animation.Sampler.Linear ->
            Linear

        Internal.Animation.Sampler.Step ->
            Step

        Internal.Animation.Sampler.Cubicspline ->
            CubicSpline


extractAnimations : Gltf -> List Animation
extractAnimations gltf =
    gltf.animations
        |> Array.toList
        |> List.map (extractAnimation gltf)


extractAnimationWithName : String -> Gltf -> List Animation
extractAnimationWithName name gltf =
    gltf.animations
        |> Array.filter (\(Internal.Animation animation) -> animation.name == Just name)
        |> Array.toList
        |> List.map (extractAnimation gltf)


extractChannel : Array Sampler -> Internal.Animation.Channel.Channel -> Maybe Channel
extractChannel samplers (Internal.Animation.Channel.Channel channel) =
    let
        (Internal.Animation.Sampler.Index samplerIndex) =
            channel.sampler
    in
    samplers
        |> Array.get samplerIndex
        |> Maybe.map
            (\sampler ->
                Channel
                    { sampler = sampler
                    , nodeIndex = channel.target.node |> (\(Internal.Node.Index i) -> NodeIndex i)
                    , path = pathFromChannel channel.target.path
                    }
            )


extractSampler : Gltf -> Internal.Animation.Sampler.Sampler -> Maybe Sampler
extractSampler gltf (Internal.Animation.Sampler.Sampler sampler) =
    Maybe.map2
        (\input output ->
            Sampler
                { input = input
                , output = output
                , interpolation = interpolationFromSampler sampler.interpolation
                }
        )
        (sampler.input
            |> Common.accessorAtIndex gltf
            |> Maybe.andThen
                (\accessor ->
                    Common.bufferViewAtIndex gltf accessor.bufferView
                        |> Maybe.map (Tuple.pair accessor)
                )
            |> Maybe.andThen
                (\( accessor, bufferView ) ->
                    Common.bufferAtIndex gltf bufferView.buffer
                        |> Maybe.map (\buffer -> Attribute.parseBuffer ( accessor, bufferView, buffer ))
                )
        )
        (sampler.output
            |> Common.accessorAtIndex gltf
            |> Maybe.andThen
                (\accessor ->
                    Common.bufferViewAtIndex gltf accessor.bufferView
                        |> Maybe.map (Tuple.pair accessor)
                )
            |> Maybe.andThen
                (\( accessor, bufferView ) ->
                    Common.bufferAtIndex gltf bufferView.buffer
                        |> Maybe.map (\buffer -> Attribute.parseBuffer ( accessor, bufferView, buffer ))
                )
        )


extractAnimation : Gltf -> Internal.Animation -> Animation
extractAnimation gltf (Internal.Animation x) =
    let
        samplers : Array Sampler
        samplers =
            x.samplers
                |> Array.toList
                |> List.filterMap (extractSampler gltf)
                |> Array.fromList
    in
    Animation
        { name = x.name
        , samplers = samplers
        , channels =
            x.channels
                |> Array.toList
                |> List.filterMap (extractChannel samplers)
        }


primitiveTreeFromNode : Gltf -> Node -> Tree ( Node, Primitive )
primitiveTreeFromNode gltf (Node node_) =
    node_.children
        |> List.filterMap (Common.nodeAtIndex gltf)
        |> primitiveTreeFromNodes gltf (Node node_)


primitiveTreeFromNodes : Gltf -> Node -> List Node -> Tree ( Node, Primitive )
primitiveTreeFromNodes gltf parentNode nodes =
    let
        parentNodeFirstPrimitive : Node -> Primitive
        parentNodeFirstPrimitive (Node x) =
            x.meshIndex
                |> Maybe.andThen (Common.meshAtIndex gltf)
                |> Maybe.andThen (.primitives >> List.head)
                |> Maybe.withDefault (Primitive [] (Just (Accessor.Index -1)) Nothing)
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
                        Tree.singleton ( node__, Primitive [] (Just (Accessor.Index -1)) Nothing )
            )
        |> Tree.tree ( parentNode, parentNodeFirstPrimitive parentNode )


nodeToPrimitives : Gltf -> Node -> ( ( Node, List Primitive ), List (Tree ( Node, Primitive )) )
nodeToPrimitives gltf (Node node_) =
    ( ( Node node_
      , node_.meshIndex
            |> Maybe.andThen (Common.meshAtIndex gltf)
            |> Maybe.map .primitives
            |> Maybe.withDefault []
      )
    , node_.children
        |> List.filterMap (Common.nodeAtIndex gltf)
        |> List.map (primitiveTreeFromNode gltf)
    )
