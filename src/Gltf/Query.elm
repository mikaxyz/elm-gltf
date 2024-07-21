module Gltf.Query exposing
    ( Error(..), Node(..), Properties(..)
    , QueryResult, QueryResultEffect
    , defaultSceneQuery, sceneQuery
    , queryResultRun, applyQueryResultEffect, applyQueryResult
    , animations, cameras, nodeTrees, skins
    , cameraByIndex, textureWithIndex
    , Gltf
    )

{-| Query contents of Gltf


# Types

@docs Error, Node, Properties
@docs QueryResult, QueryResultEffect


# Queries

@docs defaultSceneQuery, sceneQuery
@docs queryResultRun, applyQueryResultEffect, applyQueryResult


# Content

@docs animations, cameras, nodeTrees, skins
@docs cameraByIndex, textureWithIndex


# Internal

@docs Gltf

-}

import Array
import Common
import Gltf.Animation exposing (Animation)
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.Camera as Camera exposing (Camera)
import Gltf.Query.Material
import Gltf.Query.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.Skin as Skin exposing (Skin)
import Gltf.Query.SkinHelper as SkinHelper
import Gltf.Query.Task
import Gltf.Query.TextureIndex as TextureIndex
import Gltf.Query.TextureStore as TextureStore exposing (TextureStore)
import Gltf.Query.Transform exposing (Transform)
import Gltf.Query.TriangularMesh exposing (TriangularMesh)
import Gltf.Query.TriangularMeshHelper as TriangularMeshHelper
import Internal.Gltf
import Internal.Image
import Internal.Node
import Internal.Sampler
import Internal.Scene as Scene exposing (Scene(..))
import Internal.Skin
import Task
import Tree exposing (Tree)
import WebGL.Texture


{-| Type representing raw data from a .gltf/.glb file
-}
type alias Gltf =
    Internal.Gltf.Gltf


{-| All possible failures
-}
type Error
    = SceneNotFound
    | NodeNotFound


{-| TODO: Docs
-}
type Node
    = EmptyNode Properties
    | CameraNode Camera.Index Properties
    | MeshNode (List TriangularMesh) Properties
    | SkinnedMeshNode (List TriangularMesh) Skin.Index Properties


{-| TODO: Docs
-}
type Properties
    = Properties
        { nodeIndex : NodeIndex
        , nodeName : Maybe String
        , transform : Transform
        }


{-| TODO: Docs
-}
type QueryResult
    = QueryResult Gltf TextureStore (List (Tree Node))


{-| TODO: Docs
-}
type QueryResultEffect
    = QueryResultLoadTextureEffect Gltf.Query.Material.TextureIndex Internal.Image.Image (Maybe Internal.Sampler.Sampler)


{-| TODO: Docs
-}
defaultSceneQuery : Gltf -> Result Error QueryResult
defaultSceneQuery gltf =
    sceneQuery (gltf.scene |> (\(Scene.Index index) -> index)) gltf


{-| TODO: Needed?
-}
sceneQuery : Int -> Gltf -> Result Error QueryResult
sceneQuery index gltf =
    Common.sceneAtIndex gltf (Scene.Index index)
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\(Internal.Node.Index nodeIndex) -> nodeTree nodeIndex gltf |> Result.toMaybe)
                    |> List.map (Tree.map (nodeFromNode gltf))
                    |> QueryResult gltf TextureStore.init
            )
        |> Result.fromMaybe SceneNotFound


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
            case TriangularMeshHelper.toMaterial mesh of
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
                                                            (textureIndex |> TextureIndex.toImageIndex |> Common.imageAtIndex gltf)
                                                                |> Maybe.map
                                                                    (imageEffect textureIndex
                                                                        (textureIndex
                                                                            |> TextureIndex.toSamplerIndex
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
applyQueryResult : Gltf.Query.Material.TextureIndex -> Result WebGL.Texture.Error WebGL.Texture.Texture -> QueryResult -> QueryResult
applyQueryResult textureIndex textureLoadResult (QueryResult gltf textureStore queryResult) =
    case textureLoadResult of
        Ok texture ->
            QueryResult gltf (TextureStore.insert textureIndex texture textureStore) queryResult

        Err _ ->
            QueryResult gltf textureStore queryResult


{-| TODO: Docs
-}
animations : QueryResult -> List Animation
animations (QueryResult gltf _ _) =
    AnimationHelper.extractAnimations gltf


{-| TODO: Docs
-}
cameras : QueryResult -> List Camera
cameras (QueryResult gltf _ _) =
    gltf.cameras |> Array.toList


{-| TODO: Needed?
-}
skins : QueryResult -> List Skin
skins (QueryResult gltf _ _) =
    gltf.skins
        |> Array.toIndexedList
        |> List.map (Tuple.first >> Skin.Index)
        |> List.filterMap (SkinHelper.skinAtIndex gltf)


{-| TODO: Docs
-}
nodeTrees : QueryResult -> List (Tree Node)
nodeTrees (QueryResult _ _ nodes) =
    nodes


{-| TODO: Docs
-}
cameraByIndex : Camera.Index -> QueryResult -> Maybe Camera
cameraByIndex (Camera.Index index) (QueryResult gltf _ _) =
    Array.get index gltf.cameras


{-| TODO: Needed?
-}
textureWithIndex : QueryResult -> Gltf.Query.Material.TextureIndex -> Maybe WebGL.Texture.Texture
textureWithIndex (QueryResult _ textureStore _) textureIndex =
    TextureStore.textureWithTextureIndex textureIndex textureStore



--------------------------------------------------- INTERNAL


nodeIndexFromNode : Internal.Node.Index -> NodeIndex
nodeIndexFromNode (Internal.Node.Index index) =
    NodeIndex index


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


{-| TODO: Docs
-}
nodeFromNode : Gltf -> Internal.Node.Node -> Node
nodeFromNode gltf node =
    case node |> (\(Internal.Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Internal.Node.Node x) -> x.cameraIndex) of
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


propertiesFromNode : Internal.Node.Node -> Properties
propertiesFromNode (Internal.Node.Node node) =
    Properties
        { nodeIndex = nodeIndexFromNode node.index
        , nodeName = node.name
        , transform = node.transform
        }


{-| TODO: Needed?
-}
nodeTree : Int -> Gltf -> Result Error (Tree Internal.Node.Node)
nodeTree index gltf =
    Common.maybeNodeTree gltf (Internal.Node.Index index) |> Result.fromMaybe NodeNotFound


{-| TODO: Needed?
-}
triangularMeshesFromNode : Gltf -> Internal.Node.Node -> Maybe (List TriangularMesh)
triangularMeshesFromNode gltf (Internal.Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (TriangularMeshHelper.fromPrimitive gltf)
            )
