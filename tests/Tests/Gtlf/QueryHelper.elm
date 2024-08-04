module Tests.Gtlf.QueryHelper exposing
    ( Error(..)
    , fromJson
    , meshesFromNode
    , nodeTree
    , sceneNodeTrees
    , treeFromNode
    )

import Common
import Gltf
import Gltf.Mesh exposing (Mesh)
import Gltf.Node
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.BufferStore as BufferStore
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
    | GltfError Gltf.Error


fromJson : String -> (Gltf -> Result Gltf.Error b) -> Result Error b
fromJson json f =
    json
        |> JD.decodeString (Gltf.decoder "")
        |> Result.mapError DecodeError
        |> Result.andThen (\gltf -> f gltf |> Result.mapError GltfError)


meshesFromNode : Gltf.Node.Node -> List Mesh
meshesFromNode node =
    case node of
        Gltf.Node.Empty _ ->
            []

        Gltf.Node.Camera _ _ ->
            []

        Gltf.Node.Mesh triangularMeshes _ ->
            triangularMeshes

        Gltf.Node.SkinnedMesh triangularMeshes _ _ ->
            triangularMeshes

        Gltf.Node.Bone _ _ _ ->
            []


nodeTree : Int -> Gltf -> Result Gltf.Error (Tree Node)
nodeTree index gltf =
    Common.maybeNodeTree gltf (Node.Index index) |> Result.fromMaybe Gltf.NodeNotFound


sceneNodeTrees : Int -> Gltf -> Result Gltf.Error (List (Tree Node))
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
        |> Result.fromMaybe Gltf.SceneNotFound


treeFromNode : Node.Index -> Gltf -> Result Gltf.Error (Tree Gltf.Node.Node)
treeFromNode index gltf =
    Common.maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe Gltf.NodeNotFound


nodeFromNode : Gltf -> Node -> Gltf.Node.Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Gltf.Node.SkinnedMesh (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> Gltf.Node.Camera cameraIndex

                Nothing ->
                    case triangularMeshesFromNode gltf node of
                        Just meshes ->
                            node
                                |> propertiesFromNode
                                |> Gltf.Node.Mesh meshes

                        Nothing ->
                            node
                                |> propertiesFromNode
                                |> Gltf.Node.Empty


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


triangularMeshesFromNode : Gltf -> Node -> Maybe (List Mesh)
triangularMeshesFromNode gltf (Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (MeshHelper.fromPrimitive gltf (BufferStore.init gltf))
            )
