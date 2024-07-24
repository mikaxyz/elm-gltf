module Gltf exposing
    ( Asset, Error(..), Query, QueryResult
    , Msg, Gltf, init, update
    , getBinary, getEmbedded, getBinaryWithQuery, getEmbeddedWithQuery
    , defaultSceneQuery, sceneQuery
    , animations, cameras, nodeTrees, skins, cameraByIndex, textureWithIndex
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format


# Types

@docs Asset, Error, Query, QueryResult


# TEA

@docs Msg, Gltf, init, update


# Load content

@docs getBinary, getEmbedded, getBinaryWithQuery, getEmbeddedWithQuery


# Queries

@docs defaultSceneQuery, sceneQuery


# Content

@docs animations, cameras, nodeTrees, skins, cameraByIndex, textureWithIndex

-}

import Array
import Common
import Gltf.Animation exposing (Animation)
import Gltf.Camera exposing (Camera)
import Gltf.Material
import Gltf.Mesh exposing (Mesh)
import Gltf.Node exposing (Node)
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.MeshHelper as MeshHelper
import Gltf.Query.SkinHelper as SkinHelper
import Gltf.Query.Task
import Gltf.Query.TextureIndex as TextureIndex
import Gltf.Query.TextureStore as TextureStore exposing (TextureStore)
import Gltf.Skin exposing (Skin)
import Http
import Internal.Gltf
import Internal.Image
import Internal.Node
import Internal.Sampler
import Internal.Scene as Scene exposing (Scene(..))
import Internal.Skin
import Task
import Tree exposing (Tree)
import WebGL.Texture



--------------------------------------------------- Types


{-| Information about the file
-}
type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


{-| All possible failures
-}
type Error
    = HttpError Http.Error
    | TextureError WebGL.Texture.Error
    | SceneNotFound
    | NodeNotFound


{-| Type returned by [queries](Gltf#queries).
-}
type Query
    = Query (Internal.Gltf.Gltf -> Result Error QueryResult)


{-| Results from a [query](Gltf#queries) which can be used to retrieve [content](Gltf#content).
-}
type QueryResult
    = QueryResult Internal.Gltf.Gltf TextureStore (List (Tree Node))



--------------------------------------------------- TEA


{-| Internal Msg type. Handle these in your [update](Gltf#update) function. Your application Msg type should contain:

    type Msg
        = GltfMsg Gltf.Msg

-}
type Msg
    = GltfLoaded (Result Error QueryResult)
    | ApplyQueryResultEffect QueryResultEffect
    | TextureLoaded Gltf.Material.TextureIndex (Result WebGL.Texture.Error WebGL.Texture.Texture)


{-| Internal state. Your application model should contain one:

    type alias Model =
        { gltf : Gltf
        }

-}
type Gltf
    = Init
    | Loaded (Result Error QueryResult)


{-| Initialize a [Gltf](Gltf#Gltf) and store it in your model:

    init : Model
    init =
        { gltf = Gltf.init
        }

-}
init : Gltf
init =
    Init


{-| Internal update function. Call it in your update something like this:

    GltfMsg msg_ ->
        Gltf.update
            { toMsg = GltfMsg
            , onComplete = GltfOnComplete
            }
            msg_
            model.gltf
            |> Tuple.mapFirst (\gltf -> { model | gltf = gltf })

-}
update :
    { toMsg : Msg -> msg
    , onComplete : Result Error QueryResult -> msg
    }
    -> Msg
    -> Gltf
    -> ( Gltf, Cmd msg )
update { toMsg, onComplete } msg model =
    case model of
        Init ->
            case msg of
                GltfLoaded (Ok queryResult) ->
                    ( Loaded (Ok queryResult)
                    , case loadTextures queryResult of
                        Just cmd ->
                            cmd |> Cmd.map toMsg

                        Nothing ->
                            Ok queryResult |> Task.succeed |> Task.perform onComplete
                    )

                GltfLoaded (Err error) ->
                    ( model
                    , Err error |> Task.succeed |> Task.perform onComplete
                    )

                _ ->
                    ( model, Cmd.none )

        Loaded (Ok queryResult) ->
            updateLoaded toMsg onComplete msg queryResult
                |> Tuple.mapFirst (Ok >> Loaded)

        Loaded (Err error) ->
            ( model
            , Err error |> Task.succeed |> Task.perform onComplete
            )


updateLoaded :
    (Msg -> msg)
    -> (Result Error QueryResult -> msg)
    -> Msg
    -> QueryResult
    -> ( QueryResult, Cmd msg )
updateLoaded toMsg onComplete msg (QueryResult gltf textureStore nodes) =
    case msg of
        TextureLoaded textureIndex result ->
            case result of
                Ok texture ->
                    let
                        textureStore_ : TextureStore
                        textureStore_ =
                            TextureStore.insert textureIndex texture textureStore

                        queryResult : QueryResult
                        queryResult =
                            QueryResult gltf textureStore_ nodes
                    in
                    ( queryResult
                    , if TextureStore.isComplete textureStore_ then
                        Ok queryResult |> Task.succeed |> Task.perform onComplete

                      else
                        Cmd.none
                    )

                Err error ->
                    ( QueryResult gltf textureStore nodes
                    , error |> TextureError |> Err |> Task.succeed |> Task.perform onComplete
                    )

        ApplyQueryResultEffect effect ->
            case effect of
                QueryResultLoadTextureEffect textureIndex image maybeSampler ->
                    case TextureStore.get textureIndex textureStore of
                        Just _ ->
                            ( QueryResult gltf textureStore nodes, Cmd.none )

                        Nothing ->
                            case Gltf.Query.Task.loadTextureTask gltf image maybeSampler of
                                Just task ->
                                    ( QueryResult gltf (TextureStore.insertLoading textureIndex textureStore) nodes
                                    , Task.attempt (TextureLoaded textureIndex) task |> Cmd.map toMsg
                                    )

                                Nothing ->
                                    ( QueryResult gltf textureStore nodes, Cmd.none )

        _ ->
            ( QueryResult gltf textureStore nodes, Cmd.none )


loadTextures : QueryResult -> Maybe (Cmd Msg)
loadTextures (QueryResult gltf textureStore trees) =
    let
        imageEffect : Gltf.Material.TextureIndex -> Maybe Internal.Sampler.Sampler -> Internal.Image.Image -> QueryResultEffect
        imageEffect id maybeSampler image =
            QueryResultLoadTextureEffect id image maybeSampler

        textureSourceCmd : Mesh -> List (Cmd Msg)
        textureSourceCmd mesh =
            case MeshHelper.toMaterial mesh of
                Just (Gltf.Material.Material m) ->
                    let
                        maybeEffect : Maybe Gltf.Material.TextureIndex -> Maybe QueryResultEffect
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

                        perform : QueryResultEffect -> Cmd Msg
                        perform effect =
                            Task.perform ApplyQueryResultEffect (Task.succeed effect)
                    in
                    [ maybeEffect m.pbrMetallicRoughness.baseColorTexture |> Maybe.map perform
                    , maybeEffect m.pbrMetallicRoughness.metallicRoughnessTexture |> Maybe.map perform
                    , maybeEffect m.normalTexture |> Maybe.map perform
                    , maybeEffect m.occlusionTexture |> Maybe.map perform
                    , maybeEffect m.emissiveTexture |> Maybe.map perform
                    ]
                        |> List.filterMap identity

                Nothing ->
                    []

        doTree : Tree Node -> List (Cmd Msg)
        doTree tree =
            tree
                |> Tree.flatten
                |> List.concatMap meshesFromNode
                |> List.concatMap textureSourceCmd
    in
    case trees |> List.concatMap doTree of
        [] ->
            Nothing

        cmds_ ->
            cmds_ |> Cmd.batch |> Just


type QueryResultEffect
    = QueryResultLoadTextureEffect Gltf.Material.TextureIndex Internal.Image.Image (Maybe Internal.Sampler.Sampler)



--------------------------------------------------- Load content


{-| Get default scene from a file of type **.glb**

**Note:** If file does not specify a default scene this returns the first in list

-}
getBinary : String -> (Msg -> msg) -> Cmd msg
getBinary url msg =
    getBinaryWithQuery url defaultSceneQuery msg


{-| Get default scene from a file of type **.gltf**

**Note:** If file does not specify a default scene this returns the first in list

-}
getEmbedded : String -> (Msg -> msg) -> Cmd msg
getEmbedded url msg =
    getEmbeddedWithQuery url defaultSceneQuery msg


{-| Get content from a file of type **.glb** by supplying one of following queries:

  - [Gltf.defaultSceneQuery](Gltf#defaultSceneQuery)
  - [Gltf.sceneQuery](Gltf#sceneQuery)

-}
getBinaryWithQuery :
    String
    -> Query
    -> (Msg -> msg)
    -> Cmd msg
getBinaryWithQuery url (Query query) toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectBytes
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen query
                        |> GltfLoaded
                        |> toMsg
                )
                Internal.Gltf.bytesDecoder
        }


{-| Get content from a file of type **.gltf** by supplying one of following queries:

  - [Gltf.defaultSceneQuery](Gltf#defaultSceneQuery)
  - [Gltf.sceneQuery](Gltf#sceneQuery)

-}
getEmbeddedWithQuery :
    String
    -> Query
    -> (Msg -> msg)
    -> Cmd msg
getEmbeddedWithQuery url (Query query) toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> Result.andThen query
                        |> GltfLoaded
                        |> toMsg
                )
                Internal.Gltf.decoder
        }



--------------------------------------------------- Queries


{-| TODO: Docs
-}
defaultSceneQuery : Query
defaultSceneQuery =
    Query (\gltf -> sceneAtIndex gltf.scene gltf)


{-| TODO: Docs
-}
sceneQuery : Int -> Query
sceneQuery index =
    Query (sceneAtIndex (Scene.Index index))


sceneAtIndex : Scene.Index -> Internal.Gltf.Gltf -> Result Error QueryResult
sceneAtIndex index gltf =
    Common.sceneAtIndex gltf index
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\(Internal.Node.Index nodeIndex) -> nodeTree nodeIndex gltf |> Result.toMaybe)
                    |> List.map (Tree.map (nodeFromNode gltf))
                    |> QueryResult gltf TextureStore.init
            )
        |> Result.fromMaybe SceneNotFound



--------------------------------------------------- Content


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


{-| TODO: Docs
-}
skins : QueryResult -> List Skin
skins (QueryResult gltf _ _) =
    gltf.skins
        |> Array.toIndexedList
        |> List.map (Tuple.first >> Gltf.Skin.Index)
        |> List.filterMap (SkinHelper.skinAtIndex gltf)


{-| TODO: Docs
-}
nodeTrees : QueryResult -> List (Tree Node)
nodeTrees (QueryResult _ _ nodes) =
    nodes


{-| TODO: Docs
-}
cameraByIndex : Gltf.Camera.Index -> QueryResult -> Maybe Camera
cameraByIndex (Gltf.Camera.Index index) (QueryResult gltf _ _) =
    Array.get index gltf.cameras


{-| TODO: Docs
-}
textureWithIndex : QueryResult -> Gltf.Material.TextureIndex -> Maybe WebGL.Texture.Texture
textureWithIndex (QueryResult _ textureStore _) textureIndex =
    TextureStore.textureWithTextureIndex textureIndex textureStore



--------------------------------------------------- INTERNAL


nodeIndexFromNode : Internal.Node.Index -> NodeIndex
nodeIndexFromNode (Internal.Node.Index index) =
    NodeIndex index


meshesFromNode : Node -> List Mesh
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


nodeFromNode : Internal.Gltf.Gltf -> Internal.Node.Node -> Node
nodeFromNode gltf node =
    case node |> (\(Internal.Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Gltf.Node.SkinnedMeshNode (triangularMeshesFromNode gltf node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Internal.Node.Node x) -> x.cameraIndex) of
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


propertiesFromNode : Internal.Node.Node -> Gltf.Node.Properties
propertiesFromNode (Internal.Node.Node node) =
    Gltf.Node.Properties
        { nodeIndex = nodeIndexFromNode node.index
        , nodeName = node.name
        , transform = node.transform
        }


nodeTree : Int -> Internal.Gltf.Gltf -> Result Error (Tree Internal.Node.Node)
nodeTree index gltf =
    Common.maybeNodeTree gltf (Internal.Node.Index index) |> Result.fromMaybe NodeNotFound


triangularMeshesFromNode : Internal.Gltf.Gltf -> Internal.Node.Node -> Maybe (List Mesh)
triangularMeshesFromNode gltf (Internal.Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (MeshHelper.fromPrimitive gltf)
            )
