module Gltf.Query.Animation exposing
    ( ExtractedAnimation(..)
    , ExtractedChannel(..)
    , ExtractedSampler(..)
    , extractAnimationWithName
    , extractAnimations
    )

import Array exposing (Array)
import Common
import Gltf exposing (Gltf)
import Gltf.Query.Attribute as Attribute exposing (Attribute)
import Internal.Accessor as Accessor
import Internal.Animation exposing (Animation(..))
import Internal.Animation.Channel as Channel exposing (Channel(..))
import Internal.Animation.Sampler as Sampler exposing (Sampler(..))
import Internal.Mesh exposing (Primitive)
import Internal.Node as Node exposing (Node(..))
import Tree exposing (Tree)


type ExtractedAnimation
    = ExtractedAnimation
        { name : Maybe String
        , samplers : Array ExtractedSampler
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
