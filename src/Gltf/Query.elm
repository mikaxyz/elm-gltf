module Gltf.Query exposing
    ( Effect
    , Error(..)
    , Node(..)
    , Properties(..)
    , QueryError(..)
    , applyEffect
    , effectsFromNodeTree
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
import Gltf.Query.Material
import Gltf.Query.ResolvedMaterial
import Gltf.Query.Skin as Skin exposing (Skin)
import Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh)
import Internal.Material
import Internal.Mesh as Mesh exposing (Mesh)
import Internal.Node as Node
import Internal.Scene as Scene exposing (Scene(..))
import Json.Decode as JD
import Task
import Tree exposing (Tree)
import WebGL.Texture


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


type Effect
    = Noop
    | ResolveMaterial ResolveMaterialEffect


type ResolveMaterialEffect
    = ResolveMaterialEffect Gltf.Query.ResolvedMaterial.Material


applyEffect : Effect -> Tree Node -> Tree Node
applyEffect effect nodes =
    case effect of
        Noop ->
            nodes

        ResolveMaterial x ->
            applyResolveMaterialEffect x nodes


applyResolveMaterialEffect : ResolveMaterialEffect -> Tree Node -> Tree Node
applyResolveMaterialEffect (ResolveMaterialEffect (Gltf.Query.ResolvedMaterial.Material resolved)) nodes =
    let
        applyToMaterial : TriangularMesh.Material -> TriangularMesh.Material
        applyToMaterial material =
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    if unresolvedMaterial.index == resolved.index then
                        TriangularMesh.ResolvedMaterial (Gltf.Query.ResolvedMaterial.Material resolved)

                    else
                        material

                TriangularMesh.ResolvedMaterial _ ->
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
                    CameraNode _ ->
                        node

                    EmptyNode _ ->
                        node

                    MeshNode meshes a ->
                        MeshNode (meshes |> List.map applyToMesh) a

                    SkinnedMeshNode meshes a b ->
                        SkinnedMeshNode (meshes |> List.map applyToMesh) a b
            )


effectsFromNodeTree : (Effect -> msg) -> Tree Node -> Cmd msg
effectsFromNodeTree msg nodes =
    let
        toMeshes : Node -> Maybe (List TriangularMesh)
        toMeshes node =
            case node of
                CameraNode _ ->
                    Nothing

                EmptyNode _ ->
                    Nothing

                MeshNode meshes _ ->
                    Just meshes

                SkinnedMeshNode meshes _ _ ->
                    Just meshes

        updateMaterialWithIndex : Internal.Material.Index -> TriangularMesh.Material -> WebGL.Texture.Texture -> Maybe Effect
        updateMaterialWithIndex materialIndex material texture =
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    if unresolvedMaterial.index == materialIndex then
                        Gltf.Query.Material.Material unresolvedMaterial
                            |> Gltf.Query.ResolvedMaterial.fromUnresolved texture
                            |> ResolveMaterialEffect
                            |> ResolveMaterial
                            |> Just

                    else
                        Nothing

                TriangularMesh.ResolvedMaterial _ ->
                    Nothing

        cmdFromMaterial : TriangularMesh.Material -> Maybe (Cmd msg)
        cmdFromMaterial material =
            case material of
                TriangularMesh.Material (Gltf.Query.Material.Material unresolvedMaterial) ->
                    case unresolvedMaterial.pbrMetallicRoughness.baseColorTexture of
                        Just (Gltf.Query.Material.DataUri dataUri) ->
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
                                )
                                |> Just

                        Just (Gltf.Query.Material.Uri _) ->
                            -- TODO: Report Effect ResolveMaterialError?
                            Nothing

                        Nothing ->
                            Nothing

                TriangularMesh.ResolvedMaterial _ ->
                    Nothing

        meshEffect : TriangularMesh -> Maybe (Cmd msg)
        meshEffect mesh =
            case mesh of
                TriangularMesh.TriangularMesh material _ ->
                    material |> Maybe.andThen cmdFromMaterial

                TriangularMesh.IndexedTriangularMesh material _ ->
                    material |> Maybe.andThen cmdFromMaterial
    in
    nodes
        |> Tree.flatten
        |> List.filterMap toMeshes
        |> List.concat
        |> List.filterMap meshEffect
        |> Cmd.batch


nodeFromNode : Gltf -> Node.Node -> Node
nodeFromNode gltf node =
    case node |> (\(Node.Node { skinIndex }) -> skinIndex) |> Maybe.andThen (Skin.skinAtIndex gltf) of
        Just skin ->
            node
                |> propertiesFromNode
                |> SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skin

        Nothing ->
            case node |> (\(Node.Node x) -> x.cameraIndex) of
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
