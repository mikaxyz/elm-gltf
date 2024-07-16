module Gltf.Query exposing
    ( Error(..), Node(..), Properties(..), QueryError(..)
    , fromJson, nodeTree, sceneNodeTrees
    , nodeFromNode, treeFromNode, skinFromNode, triangularMeshesFromNode, meshesFromNode
    , QueryResult, QueryResultEffect
    , textureWithIndex, queryResultRun, queryResultNodes, sceneQuery, applyQueryResultEffect, applyQueryResult
    )

{-| Query contents of Gltf

@docs Error, Node, Properties, QueryError
@docs fromJson, nodeTree, sceneNodeTrees
@docs nodeFromNode, treeFromNode, skinFromNode, triangularMeshesFromNode, meshesFromNode
@docs QueryResult, QueryResultEffect
@docs textureWithIndex, queryResultRun, queryResultNodes, sceneQuery, applyQueryResultEffect, applyQueryResult

-}

import Common
import Gltf exposing (Gltf)
import Gltf.Query.Material
import Gltf.Query.Skin as Skin exposing (Skin)
import Gltf.Query.Task
import Gltf.Query.TextureStore as TextureStore exposing (TextureStore)
import Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh)
import Internal.Camera
import Internal.Image
import Internal.Node as Node
import Internal.Sampler
import Internal.Scene as Scene exposing (Scene(..))
import Json.Decode as JD
import Task
import Tree exposing (Tree)
import WebGL.Texture


{-| TODO: Needed?
-}
type Error
    = DecodeError JD.Error
    | QueryError QueryError


{-| TODO: Needed?
-}
fromJson : String -> (Gltf -> Result QueryError b) -> Result Error b
fromJson json f =
    json
        |> JD.decodeString Gltf.decoder
        |> Result.mapError DecodeError
        |> Result.andThen (\gltf -> f gltf |> Result.mapError QueryError)


{-| TODO: Needed?
-}
type QueryError
    = SceneNotFound
    | NodeNotFound


{-| TODO: Docs
-}
type Node
    = EmptyNode Properties
    | CameraNode Internal.Camera.Index Properties
    | MeshNode (List TriangularMesh) Properties
    | SkinnedMeshNode (List TriangularMesh) Skin Properties


{-| TODO: Needed?
-}
meshesFromNode : Node -> List TriangularMesh
meshesFromNode node =
    case node of
        EmptyNode _ ->
            []

        CameraNode _ _ ->
            []

        MeshNode triangularMeshes _ ->
            triangularMeshes

        SkinnedMeshNode triangularMeshes _ _ ->
            triangularMeshes


{-| TODO: Needed?
-}
skinFromNode : Node -> Maybe Skin
skinFromNode node =
    case node of
        EmptyNode _ ->
            Nothing

        CameraNode _ _ ->
            Nothing

        MeshNode _ _ ->
            Nothing

        SkinnedMeshNode _ skin _ ->
            Just skin


{-| TODO: Needed?
-}
treeFromNode : Node.Index -> Gltf -> Result QueryError (Tree Node)
treeFromNode index gltf =
    maybeNodeTree gltf index
        |> Maybe.map (Tree.map (nodeFromNode gltf))
        |> Result.fromMaybe NodeNotFound


type QueryResult
    = QueryResult Gltf TextureStore (List (Tree Node))


type QueryResultEffect
    = QueryResultLoadTextureEffect Gltf.Query.Material.TextureIndex Internal.Image.Image (Maybe Internal.Sampler.Sampler)


queryResultNodes : QueryResult -> List (Tree Node)
queryResultNodes (QueryResult _ _ nodes) =
    nodes


{-| TODO: Needed?
-}
textureWithIndex : QueryResult -> Gltf.Query.Material.TextureIndex -> Maybe WebGL.Texture.Texture
textureWithIndex (QueryResult _ textureStore _) textureIndex =
    TextureStore.textureWithTextureIndex textureIndex textureStore


{-| TODO: Needed?
-}
sceneQuery : Scene.Index -> Gltf -> Result QueryError QueryResult
sceneQuery index gltf =
    Common.sceneAtIndex gltf index
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\nodeIndex -> nodeTree nodeIndex gltf |> Result.toMaybe)
                    |> List.map (Tree.map (nodeFromNode gltf))
                    |> QueryResult gltf TextureStore.init
            )
        |> Result.fromMaybe SceneNotFound


{-| TODO: Docs
-}
applyQueryResult : Gltf.Query.Material.TextureIndex -> Result WebGL.Texture.Error WebGL.Texture.Texture -> QueryResult -> QueryResult
applyQueryResult textureIndex textureLoadResult (QueryResult gltf textureStore queryResult) =
    case textureLoadResult of
        Ok texture ->
            QueryResult gltf (TextureStore.insert textureIndex texture textureStore) queryResult

        Err _ ->
            QueryResult gltf textureStore queryResult


{-| TODO: Docs
-}
applyQueryResultEffect :
    QueryResultEffect
    -> (Gltf.Query.Material.TextureIndex -> Result WebGL.Texture.Error WebGL.Texture.Texture -> msg)
    -> QueryResult
    -> ( QueryResult, Cmd msg )
applyQueryResultEffect effect msg (QueryResult gltf textureStore queryResult) =
    case effect of
        QueryResultLoadTextureEffect textureIndex image maybeSampler ->
            case TextureStore.get textureIndex textureStore of
                Just _ ->
                    ( QueryResult gltf textureStore queryResult, Cmd.none )

                Nothing ->
                    case Gltf.Query.Task.loadTextureTask gltf image maybeSampler of
                        Just task ->
                            ( QueryResult gltf (TextureStore.insertLoading textureIndex textureStore) queryResult
                            , Task.attempt (msg textureIndex) task
                            )

                        Nothing ->
                            ( QueryResult gltf textureStore queryResult, Cmd.none )


{-| TODO: Docs
-}
queryResultRun : (QueryResultEffect -> msg) -> QueryResult -> Cmd msg
queryResultRun msg (QueryResult gltf textureStore trees) =
    let
        imageEffect : Gltf.Query.Material.TextureIndex -> Maybe Internal.Sampler.Sampler -> Internal.Image.Image -> QueryResultEffect
        imageEffect id maybeSampler image =
            QueryResultLoadTextureEffect id image maybeSampler

        textureSourceCmd : TriangularMesh -> Maybe (Cmd msg)
        textureSourceCmd mesh =
            case TriangularMesh.toMaterial mesh of
                Just (Gltf.Query.Material.Material m) ->
                    let
                        maybeEffect : Maybe Gltf.Query.Material.TextureIndex -> Maybe QueryResultEffect
                        maybeEffect maybeTextureIndex =
                            maybeTextureIndex
                                |> Maybe.andThen (\textureIndex -> TextureStore.get textureIndex textureStore)
                                |> (\texture ->
                                        case texture of
                                            Just _ ->
                                                Nothing

                                            Nothing ->
                                                maybeTextureIndex
                                                    |> Maybe.andThen
                                                        (\textureIndex ->
                                                            (textureIndex |> Gltf.Query.Material.toImageIndex |> Common.imageAtIndex gltf)
                                                                |> Maybe.map
                                                                    (imageEffect textureIndex
                                                                        (textureIndex
                                                                            |> Gltf.Query.Material.toSamplerIndex
                                                                            |> Maybe.andThen (Common.samplerAtIndex gltf)
                                                                        )
                                                                    )
                                                        )
                                   )

                        perform : QueryResultEffect -> Cmd msg
                        perform effect =
                            Task.perform msg (Task.succeed effect)
                    in
                    [ maybeEffect m.pbrMetallicRoughness.baseColorTexture |> Maybe.map perform
                    , maybeEffect m.pbrMetallicRoughness.metallicRoughnessTexture |> Maybe.map perform
                    , maybeEffect m.normalTexture |> Maybe.map perform
                    , maybeEffect m.occlusionTexture |> Maybe.map perform
                    , maybeEffect m.emissiveTexture |> Maybe.map perform
                    ]
                        |> List.filterMap identity
                        |> Cmd.batch
                        |> Just

                Nothing ->
                    Nothing

        doTree : Tree Node -> List (Cmd msg)
        doTree tree =
            tree
                |> Tree.flatten
                |> List.concatMap meshesFromNode
                |> List.filterMap textureSourceCmd
    in
    trees
        |> List.concatMap doTree
        |> Cmd.batch


{-| TODO: Docs
-}
nodeFromNode : Gltf -> Node.Node -> Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.andThen (Skin.skinAtIndex gltf) of
        Just skin ->
            node
                |> propertiesFromNode
                |> SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skin

        Nothing ->
            case node |> (\(Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> CameraNode cameraIndex

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


{-| TODO: Docs
-}
type Properties
    = Properties
        { nodeIndex : Node.Index
        , nodeName : Maybe String
        , transform : Node.Transform
        }


{-| TODO: Needed?
-}
sceneNodeTrees : Scene.Index -> Gltf -> Result QueryError (List (Tree Node.Node))
sceneNodeTrees index gltf =
    Common.sceneAtIndex gltf index
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


{-| TODO: Needed?
-}
nodeTree : Node.Index -> Gltf -> Result QueryError (Tree Node.Node)
nodeTree index gltf =
    maybeNodeTree gltf index |> Result.fromMaybe NodeNotFound


maybeNodeTree : Gltf -> Node.Index -> Maybe (Tree Node.Node)
maybeNodeTree gltf index =
    Common.nodeAtIndex gltf index
        |> Maybe.map
            (\(Node.Node node_) ->
                node_.children
                    |> List.filterMap (maybeNodeTree gltf)
                    |> Tree.tree (Node.Node node_)
            )


{-| TODO: Needed?
-}
triangularMeshesFromNode : Gltf -> Node.Node -> Maybe (List TriangularMesh)
triangularMeshesFromNode gltf (Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (TriangularMesh.fromPrimitive gltf)
            )
