module Gltf.Query.AnimationHelper exposing
    ( extractAnimationWithName
    , extractAnimations
    )

import Array exposing (Array)
import Common
import Gltf.Animation.Animation exposing (Animation(..))
import Gltf.Animation.Channel as Channel exposing (Channel(..))
import Gltf.Animation.Sampler as Sampler exposing (Sampler(..))
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.Attribute as Attribute
import Gltf.Query.BufferStore exposing (BufferStore)
import Internal.Accessor as Accessor
import Internal.Animation as Internal
import Internal.Animation.Channel
import Internal.Animation.Sampler
import Internal.Gltf exposing (Gltf)
import Internal.Mesh exposing (Primitive)
import Internal.Node exposing (Node(..))
import Tree exposing (Tree)


pathFromChannel : Internal.Animation.Channel.Path -> Channel.Path
pathFromChannel channelPath =
    case channelPath of
        Internal.Animation.Channel.Translation ->
            Channel.Translation

        Internal.Animation.Channel.Rotation ->
            Channel.Rotation

        Internal.Animation.Channel.Scale ->
            Channel.Scale

        Internal.Animation.Channel.Weights ->
            Channel.Weights


interpolationFromSampler : Internal.Animation.Sampler.Interpolation -> Sampler.Interpolation
interpolationFromSampler interpolation =
    case interpolation of
        Internal.Animation.Sampler.Linear ->
            Sampler.Linear

        Internal.Animation.Sampler.Step ->
            Sampler.Step

        Internal.Animation.Sampler.Cubicspline ->
            Sampler.CubicSpline


extractAnimations : Gltf -> BufferStore -> List Animation
extractAnimations gltf bufferStore =
    gltf.animations
        |> Array.toList
        |> List.map (extractAnimation gltf bufferStore)


extractAnimationWithName : String -> Gltf -> BufferStore -> List Animation
extractAnimationWithName name gltf bufferStore =
    gltf.animations
        |> Array.filter (\(Internal.Animation animation) -> animation.name == Just name)
        |> Array.toList
        |> List.map (extractAnimation gltf bufferStore)


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


extractSampler : Gltf -> BufferStore -> Internal.Animation.Sampler.Sampler -> Maybe Sampler
extractSampler gltf bufferStore (Internal.Animation.Sampler.Sampler sampler) =
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
                    Common.bufferInfo gltf bufferStore accessor
                        |> Maybe.map Attribute.parseBuffer
                        |> Maybe.map (List.filterMap Attribute.toFloat)
                )
        )
        (sampler.output
            |> Common.accessorAtIndex gltf
            |> Maybe.andThen
                (\accessor ->
                    Common.bufferInfo gltf bufferStore accessor
                        |> Maybe.map Attribute.parseBuffer
                )
        )


extractAnimation : Gltf -> BufferStore -> Internal.Animation -> Animation
extractAnimation gltf bufferStore (Internal.Animation x) =
    let
        samplers : Array Sampler
        samplers =
            x.samplers
                |> Array.toList
                |> List.filterMap (extractSampler gltf bufferStore)
                |> Array.fromList
    in
    Animation
        { name = x.name
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
                -- TODO: Make this fail instead
                |> Maybe.withDefault (Primitive [] (Just (Accessor.Index -1)) Nothing Internal.Mesh.Triangles)
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
                        -- TODO: Make this fail instead
                        Tree.singleton ( node__, Primitive [] (Just (Accessor.Index -1)) Nothing Internal.Mesh.Triangles )
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
