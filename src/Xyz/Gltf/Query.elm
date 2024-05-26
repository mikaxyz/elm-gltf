module Xyz.Gltf.Query exposing
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
import Json.Decode as JD
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion exposing (Quaternion)
import Tree exposing (Tree)
import Xyz.Gltf.Mesh as Mesh exposing (Mesh, Primitive)
import Xyz.Gltf.Node as Node
import Xyz.Gltf.Query.Skin as Skin exposing (Skin(..))
import Xyz.Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh)
import Xyz.Gltf.Scene as Scene exposing (Scene(..))


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
    = MeshNode (List TriangularMesh) Properties
    | SkinnedMeshNode (List TriangularMesh) Skin Properties


meshesFromNode : Node -> List TriangularMesh
meshesFromNode node =
    case node of
        MeshNode triangularMeshes properties ->
            triangularMeshes

        SkinnedMeshNode triangularMeshes skin properties ->
            triangularMeshes


skinFromNode : Node -> Maybe Skin
skinFromNode node =
    case node of
        MeshNode _ _ ->
            Nothing

        SkinnedMeshNode triangularMeshes skin properties ->
            Just skin


treeFromNode : Node.Index -> Gltf -> Result QueryError (Tree Node)
treeFromNode index gltf =
    maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe NodeNotFound


nodeFromNode : Gltf -> Node.Node -> Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.andThen (Skin.skinAtIndex gltf) of
        Just skin ->
            node
                |> propertiesFromNode
                |> SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skin

        Nothing ->
            node
                |> propertiesFromNode
                |> MeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault [])


propertiesFromNode : Node.Node -> Properties
propertiesFromNode (Node.Node node) =
    Properties
        { nodeIndex = node.index
        , nodeName = node.name
        , rotation = node.rotation |> Maybe.map (\{ x, y, z, w } -> Quaternion.quaternion w x y z)
        , translation = node.translation |> Maybe.map Vec3.fromRecord
        , scale = node.scale |> Maybe.map Vec3.fromRecord
        }


type Properties
    = Properties
        { nodeIndex : Node.Index
        , nodeName : Maybe String
        , rotation : Maybe Quaternion
        , translation : Maybe Vec3
        , scale : Maybe Vec3
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
