module Gltf.QueryHelper exposing
    ( Error(..)
    , fromJson
    , meshesFromNode
    , nodeTree
    , sceneNodeTrees
    , treeFromNode
    )

import Common
import Gltf.Mesh exposing (Mesh)
import Gltf.Node
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query as Query
import Gltf.Query.MeshHelper as MeshHelper
import Gltf.Skin
import Internal.Gltf as Gltf exposing (Gltf)
import Internal.Node as Node exposing (Node)
import Internal.Scene as Scene exposing (Scene(..))
import Internal.Skin
import Json.Decode as JD
import Tree exposing (Tree)


type Error
    = DecodeError JD.Error
    | QueryError Query.Error


fromJson : String -> (Gltf -> Result Query.Error b) -> Result Error b
fromJson json f =
    json
        |> JD.decodeString Gltf.decoder
        |> Result.mapError DecodeError
        |> Result.andThen (\gltf -> f gltf |> Result.mapError QueryError)


{-| TODO: DUPE exists in Query also, Use queries in tests?
-}
meshesFromNode : Gltf.Node.Node -> List Mesh
meshesFromNode node =
    case node of
        Gltf.Node.EmptyNode _ ->
            []

        Gltf.Node.CameraNode _ _ ->
            []

        Gltf.Node.MeshNode triangularMeshes _ ->
            triangularMeshes

        Gltf.Node.SkinnedMeshNode triangularMeshes _ _ ->
            triangularMeshes


{-| TODO: DUPE exists in Query also, Use queries in tests?
-}
nodeTree : Int -> Gltf -> Result Query.Error (Tree Node)
nodeTree index gltf =
    Common.maybeNodeTree gltf (Node.Index index) |> Result.fromMaybe Query.NodeNotFound


{-| TODO: Use queries in tests?
-}
sceneNodeTrees : Int -> Gltf -> Result Query.Error (List (Tree Node))
sceneNodeTrees index gltf =
    Common.sceneAtIndex gltf (Scene.Index index)
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\(Node.Index nodeIndex) ->
                            nodeTree nodeIndex gltf
                                |> Result.toMaybe
                        )
            )
        |> Result.fromMaybe Query.SceneNotFound


{-| TODO: Use queries in tests?
-}
treeFromNode : Node.Index -> Gltf -> Result Query.Error (Tree Gltf.Node.Node)
treeFromNode index gltf =
    Common.maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe Query.NodeNotFound


{-| TODO: Docs
-}
nodeFromNode : Gltf -> Node -> Gltf.Node.Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Gltf.Node.SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> Gltf.Node.CameraNode cameraIndex

                Nothing ->
                    case triangularMeshesFromNode gltf node of
                        Just meshes ->
                            node
                                |> propertiesFromNode
                                |> Gltf.Node.MeshNode meshes

                        Nothing ->
                            node
                                |> propertiesFromNode
                                |> Gltf.Node.EmptyNode


propertiesFromNode : Node -> Gltf.Node.Properties
propertiesFromNode (Node.Node node) =
    Gltf.Node.Properties
        { nodeIndex = nodeIndexFromNode node.index
        , nodeName = node.name
        , transform = node.transform
        }


nodeIndexFromNode : Node.Index -> NodeIndex
nodeIndexFromNode (Node.Index index) =
    NodeIndex index


{-| TODO: Needed?
-}
triangularMeshesFromNode : Gltf -> Node -> Maybe (List Mesh)
triangularMeshesFromNode gltf (Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (MeshHelper.fromPrimitive gltf)
            )
