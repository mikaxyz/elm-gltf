module Gltf.Query exposing
    ( Error(..), Node(..), Properties(..), QueryError(..)
    , fromJson, nodeTree, sceneNodeTrees
    , Effect, effectsFromNodeTree, applyEffect
    , nodeFromNode, treeFromNode, skinFromNode, triangularMeshesFromNode, meshesFromNode
    )

{-| Query contents of Gltf

@docs Error, Node, Properties, QueryError
@docs fromJson, nodeTree, sceneNodeTrees
@docs Effect, effectsFromNodeTree, applyEffect
@docs nodeFromNode, treeFromNode, skinFromNode, triangularMeshesFromNode, meshesFromNode

-}

import Common
import Gltf exposing (Gltf)
import Gltf.Query.Material
import Gltf.Query.ResolvedMaterial
import Gltf.Query.Skin as Skin exposing (Skin)
import Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh)
import Internal.Camera
import Internal.Material
import Internal.Node as Node
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


{-| TODO: Docs
-}
type Effect
    = Noop
    | ResolveTexture ResolveTextureEffect


type ResolveTextureEffect
    = ResolveTextureEffect Internal.Material.Index (Gltf.Query.ResolvedMaterial.Texture WebGL.Texture.Texture)


{-| TODO: Docs
-}
applyEffect : Effect -> Tree Node -> Tree Node
applyEffect effect nodes =
    case effect of
        Noop ->
            nodes

        ResolveTexture texture ->
            applyResolveTextureEffect texture nodes


applyResolveTextureEffect : ResolveTextureEffect -> Tree Node -> Tree Node
applyResolveTextureEffect (ResolveTextureEffect index texture) nodes =
    let
        applyToMaterial : TriangularMesh.Material -> TriangularMesh.Material
        applyToMaterial material =
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    if unresolvedMaterial.index == index then
                        Gltf.Query.Material.Material unresolvedMaterial
                            |> Gltf.Query.ResolvedMaterial.fromUnresolved
                            |> Gltf.Query.ResolvedMaterial.updateTexture texture
                            |> TriangularMesh.ResolvedMaterial

                    else
                        material

                TriangularMesh.ResolvedMaterial (Gltf.Query.ResolvedMaterial.Material resolvedMaterial) ->
                    if resolvedMaterial.index == index then
                        Gltf.Query.ResolvedMaterial.Material resolvedMaterial
                            |> Gltf.Query.ResolvedMaterial.updateTexture texture
                            |> TriangularMesh.ResolvedMaterial

                    else
                        material

        applyToMesh : TriangularMesh -> TriangularMesh
        applyToMesh mesh =
            case mesh of
                TriangularMesh.TriangularMesh material a ->
                    TriangularMesh.TriangularMesh (material |> Maybe.map applyToMaterial) a

                TriangularMesh.IndexedTriangularMesh material a ->
                    TriangularMesh.IndexedTriangularMesh (material |> Maybe.map applyToMaterial) a
    in
    nodes
        |> Tree.map
            (\node ->
                case node of
                    CameraNode _ _ ->
                        node

                    EmptyNode _ ->
                        node

                    MeshNode meshes a ->
                        MeshNode (meshes |> List.map applyToMesh) a

                    SkinnedMeshNode meshes a b ->
                        SkinnedMeshNode (meshes |> List.map applyToMesh) a b
            )


{-| TODO: Docs
-}
effectsFromNodeTree : (Effect -> msg) -> Tree Node -> Cmd msg
effectsFromNodeTree msg nodes =
    let
        toMeshes : Node -> Maybe (List TriangularMesh)
        toMeshes node =
            case node of
                CameraNode _ _ ->
                    Nothing

                EmptyNode _ ->
                    Nothing

                MeshNode meshes _ ->
                    Just meshes

                SkinnedMeshNode meshes _ _ ->
                    Just meshes

        meshMaterial : TriangularMesh -> Maybe TriangularMesh.Material
        meshMaterial mesh =
            case mesh of
                TriangularMesh.TriangularMesh material _ ->
                    material

                TriangularMesh.IndexedTriangularMesh material _ ->
                    material

        updateMaterialWithIndex : Internal.Material.Index -> TriangularMesh.Material -> Gltf.Query.ResolvedMaterial.Texture WebGL.Texture.Texture -> Maybe Effect
        updateMaterialWithIndex materialIndex material texture =
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    if unresolvedMaterial.index == materialIndex then
                        texture
                            |> ResolveTextureEffect materialIndex
                            |> ResolveTexture
                            |> Just

                    else
                        Nothing

                TriangularMesh.ResolvedMaterial (Gltf.Query.ResolvedMaterial.Material resolvedMaterial) ->
                    if resolvedMaterial.index == materialIndex then
                        texture
                            |> ResolveTextureEffect materialIndex
                            |> ResolveTexture
                            |> Just

                    else
                        Nothing

        cmdFromMaterial : TriangularMesh.Material -> Maybe (Cmd msg)
        cmdFromMaterial material =
            let
                toCmd : Gltf.Query.Material.Material -> (WebGL.Texture.Texture -> Gltf.Query.ResolvedMaterial.Texture WebGL.Texture.Texture) -> Gltf.Query.Material.MaterialImage -> Maybe (Cmd msg)
                toCmd (Gltf.Query.Material.Material unresolvedMaterial) toTexture unresolvedTexture =
                    case unresolvedTexture of
                        Gltf.Query.Material.DataUri dataUri ->
                            let
                                defaultOptions : WebGL.Texture.Options
                                defaultOptions =
                                    WebGL.Texture.defaultOptions

                                -- TODO: Options from Sampler
                                -- flipY: https://github.com/KhronosGroup/glTF-Sample-Viewer/issues/16
                                options : WebGL.Texture.Options
                                options =
                                    { defaultOptions
                                        | flipY = False
                                    }
                            in
                            Task.attempt
                                (\result ->
                                    -- TODO: Report/return Effect Texture.Error
                                    result
                                        |> Result.toMaybe
                                        |> Maybe.andThen (updateMaterialWithIndex unresolvedMaterial.index material)
                                        |> Maybe.withDefault Noop
                                        |> msg
                                )
                                (WebGL.Texture.loadWith
                                    options
                                    dataUri
                                    |> Task.map toTexture
                                )
                                |> Just

                        Gltf.Query.Material.Uri _ ->
                            -- TODO: Report Effect ResolveMaterialError?
                            Nothing
            in
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    [ Gltf.Query.ResolvedMaterial.BaseColorTexture unresolvedMaterial.pbrMetallicRoughness.baseColorTexture
                    , Gltf.Query.ResolvedMaterial.MetallicRoughnessTexture unresolvedMaterial.pbrMetallicRoughness.metallicRoughnessTexture
                    , Gltf.Query.ResolvedMaterial.NormalTexture unresolvedMaterial.normalTexture
                    , Gltf.Query.ResolvedMaterial.OcclusionTexture unresolvedMaterial.occlusionTexture
                    , Gltf.Query.ResolvedMaterial.EmissiveTexture unresolvedMaterial.emissiveTexture
                    ]
                        |> List.filterMap
                            (\texture ->
                                case texture of
                                    Gltf.Query.ResolvedMaterial.BaseColorTexture maybeMaterialImage ->
                                        maybeMaterialImage
                                            |> Maybe.andThen
                                                (toCmd (Gltf.Query.Material.Material unresolvedMaterial) Gltf.Query.ResolvedMaterial.BaseColorTexture)

                                    Gltf.Query.ResolvedMaterial.MetallicRoughnessTexture maybeMaterialImage ->
                                        maybeMaterialImage
                                            |> Maybe.andThen
                                                (toCmd (Gltf.Query.Material.Material unresolvedMaterial) Gltf.Query.ResolvedMaterial.MetallicRoughnessTexture)

                                    Gltf.Query.ResolvedMaterial.NormalTexture maybeMaterialImage ->
                                        maybeMaterialImage
                                            |> Maybe.andThen
                                                (toCmd (Gltf.Query.Material.Material unresolvedMaterial) Gltf.Query.ResolvedMaterial.NormalTexture)

                                    Gltf.Query.ResolvedMaterial.OcclusionTexture maybeMaterialImage ->
                                        maybeMaterialImage
                                            |> Maybe.andThen
                                                (toCmd (Gltf.Query.Material.Material unresolvedMaterial) Gltf.Query.ResolvedMaterial.OcclusionTexture)

                                    Gltf.Query.ResolvedMaterial.EmissiveTexture maybeMaterialImage ->
                                        maybeMaterialImage
                                            |> Maybe.andThen
                                                (toCmd (Gltf.Query.Material.Material unresolvedMaterial) Gltf.Query.ResolvedMaterial.EmissiveTexture)
                            )
                        |> Cmd.batch
                        |> Just

                TriangularMesh.ResolvedMaterial _ ->
                    Nothing
    in
    nodes
        |> Tree.flatten
        |> List.filterMap toMeshes
        |> List.concat
        |> List.filterMap meshMaterial
        |> List.filterMap cmdFromMaterial
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
