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
import Gltf.Query as Query
import Gltf.Query.MeshHelper as MeshHelper
import Gltf.Query.NodeIndex exposing (NodeIndex(..))
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
meshesFromNode : Query.Node -> List Mesh
meshesFromNode node =
    case node of
        Query.EmptyNode _ ->
            []

        Query.CameraNode _ _ ->
            []

        Query.MeshNode triangularMeshes _ ->
            triangularMeshes

        Query.SkinnedMeshNode triangularMeshes _ _ ->
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
treeFromNode : Node.Index -> Gltf -> Result Query.Error (Tree Query.Node)
treeFromNode index gltf =
    Common.maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe Query.NodeNotFound


{-| TODO: Docs
-}
nodeFromNode : Gltf -> Node -> Query.Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Query.SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> Query.CameraNode cameraIndex

                Nothing ->
                    case triangularMeshesFromNode gltf node of
                        Just meshes ->
                            node
                                |> propertiesFromNode
                                |> Query.MeshNode meshes

                        Nothing ->
                            node
                                |> propertiesFromNode
                                |> Query.EmptyNode


propertiesFromNode : Node -> Query.Properties
propertiesFromNode (Node.Node node) =
    Query.Properties
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
