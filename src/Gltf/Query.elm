module Gltf.Query exposing
    ( Error(..)
    , Node(..)
    , Properties(..)
    , QueryError(..)
    , fromJson
    , meshesFromNode
    , nodeFromNode
    , nodeTree
    , sceneNodeTrees
    , skinFromNode
    , treeFromNode
    , triangularMeshesFromNode
    )

import Array
import Gltf exposing (Gltf)
import Gltf.Query.Skin as Skin exposing (Skin)
import Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh)
import Internal.Mesh as Mesh exposing (Mesh)
import Internal.Node as Node
import Internal.Scene as Scene exposing (Scene(..))
import Json.Decode as JD
import Tree exposing (Tree)


type Error
    = DecodeError JD.Error
    | QueryError QueryError


fromJson : String -> (Gltf -> Result QueryError b) -> Result Error b
fromJson json f =
    json
        |> JD.decodeString Gltf.decoder
        |> Result.mapError DecodeError
        |> Result.andThen (\gltf -> f gltf |> Result.mapError QueryError)


type QueryError
    = SceneNotFound
    | NodeNotFound


type Node
    = EmptyNode Properties
    | CameraNode Properties
    | MeshNode (List TriangularMesh) Properties
    | SkinnedMeshNode (List TriangularMesh) Skin Properties


meshesFromNode : Node -> List TriangularMesh
meshesFromNode node =
    case node of
        EmptyNode _ ->
            []

        CameraNode _ ->
            []

        MeshNode triangularMeshes _ ->
            triangularMeshes

        SkinnedMeshNode triangularMeshes _ _ ->
            triangularMeshes


skinFromNode : Node -> Maybe Skin
skinFromNode node =
    case node of
        EmptyNode _ ->
            Nothing

        CameraNode _ ->
            Nothing

        MeshNode _ _ ->
            Nothing

        SkinnedMeshNode _ skin _ ->
            Just skin


treeFromNode : Node.Index -> Gltf -> Result QueryError (Tree Node)
treeFromNode index gltf =
    maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe NodeNotFound


nodeFromNode : Gltf -> Node.Node -> Node
nodeFromNode gltf node =
    let
        cameraIndex =
            node |> (\(Node.Node x) -> x.cameraIndex)
    in
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.andThen (Skin.skinAtIndex gltf) of
        Just skin ->
            node
                |> propertiesFromNode
                |> SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skin

        Nothing ->
            case cameraIndex of
                Just _ ->
                    node
                        |> propertiesFromNode
                        |> CameraNode

                Nothing ->
                    case triangularMeshesFromNode gltf node of
                        Just meshes ->
                            node
                                |> propertiesFromNode
                                |> MeshNode meshes

                        Nothing ->
                            node
                                |> propertiesFromNode
                                |> EmptyNode


propertiesFromNode : Node.Node -> Properties
propertiesFromNode (Node.Node node) =
    Properties
        { nodeIndex = node.index
        , nodeName = node.name
        , transform = node.transform
        }


type Properties
    = Properties
        { nodeIndex : Node.Index
        , nodeName : Maybe String
        , transform : Node.Transform
        }


sceneNodeTrees : Scene.Index -> Gltf -> Result QueryError (List (Tree Node.Node))
sceneNodeTrees index gltf =
    sceneAtIndex gltf index
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\nodeIndex ->
                            nodeTree nodeIndex gltf
                                |> Result.toMaybe
                        )
            )
        |> Result.fromMaybe SceneNotFound


nodeTree : Node.Index -> Gltf -> Result QueryError (Tree Node.Node)
nodeTree index gltf =
    maybeNodeTree gltf index |> Result.fromMaybe NodeNotFound


maybeNodeTree : Gltf -> Node.Index -> Maybe (Tree Node.Node)
maybeNodeTree gltf index =
    nodeAtIndex gltf index
        |> Maybe.map
            (\(Node.Node node_) ->
                node_.children
                    |> List.filterMap (maybeNodeTree gltf)
                    |> Tree.tree (Node.Node node_)
            )


triangularMeshesFromNode : Gltf -> Node.Node -> Maybe (List TriangularMesh)
triangularMeshesFromNode gltf (Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (TriangularMesh.fromPrimitive gltf)
            )


sceneAtIndex : Gltf -> Scene.Index -> Maybe Scene
sceneAtIndex gltf (Scene.Index index) =
    gltf.scenes |> Array.get index


nodeAtIndex : Gltf -> Node.Index -> Maybe Node.Node
nodeAtIndex gltf (Node.Index index) =
    gltf.nodes |> Array.get index


meshAtIndex : Gltf -> Mesh.Index -> Maybe Mesh
meshAtIndex gltf (Mesh.Index index) =
    gltf.meshes |> Array.get index
