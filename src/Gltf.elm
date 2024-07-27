module Gltf exposing
    ( Asset, Error(..), Query, QueryResult
    , Msg, Gltf, init, update
    , getBinary, getGltf, getBinaryWithQuery, getGltfWithQuery
    , defaultSceneQuery, sceneQuery
    , animations, cameras, nodeTrees, skins, cameraByIndex, textureWithIndex
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format


# Types

@docs Asset, Error, Query, QueryResult


# TEA

@docs Msg, Gltf, init, update


# Load content

@docs getBinary, getGltf, getBinaryWithQuery, getGltfWithQuery


# Queries

@docs defaultSceneQuery, sceneQuery


# Content

@docs animations, cameras, nodeTrees, skins, cameraByIndex, textureWithIndex

-}

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Common
import Gltf.Animation exposing (Animation)
import Gltf.Camera exposing (Camera)
import Gltf.Material
import Gltf.Mesh exposing (Mesh)
import Gltf.Node exposing (Node)
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.BufferStore as BufferStore exposing (BufferStore)
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
    = DefaultSceneQuery
    | SceneQuery Int


{-| Results from a [query](Gltf#queries) which can be used to retrieve [content](Gltf#content).
-}
type QueryResult
    = QueryResult Query Internal.Gltf.Gltf BufferStore TextureStore (List (Tree Node))



--------------------------------------------------- TEA


{-| Internal Msg type. Handle these in your [update](Gltf#update) function. Your application Msg type should contain:

    type Msg
        = GltfMsg Gltf.Msg

-}
type Msg
    = GltfLoaded Query (Result Error Internal.Gltf.Gltf)
    | GltfProgressMsg ProgressMsg


type ProgressMsg
    = BufferLoaded (Result Error ( Int, Bytes ))
    | TextureLoaded Gltf.Material.TextureIndex (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | LoadTexture LoadTextureInfo


type alias LoadTextureInfo =
    { textureIndex : Gltf.Material.TextureIndex
    , image : Internal.Image.Image
    , maybeSampler : Maybe Internal.Sampler.Sampler
    }


{-| Internal state. Your application model should contain one:

    type alias Model =
        { gltf : Gltf
        }

-}
type Gltf
    = Initializing
    | Progress Model


type alias Model =
    { gltf : Internal.Gltf.Gltf
    , query : Query
    , bufferStore : BufferStore
    , textureStore : TextureStore
    }


{-| Initialize a [Gltf](Gltf#Gltf) and store it in your model:

    init : Model
    init =
        { gltf = Gltf.init
        }

-}
init : Gltf
init =
    Initializing


{-| Internal update function. Call it in your update something like this:

    GltfMsg msg_ ->
        Gltf.update
            { toMsg = GltfMsg
            , onComplete = GltfOnComplete
            }
            msg_
            model.gltf
            |> Tuple.mapFirst (\gltf -> { model | gltf = gltf })

The onComplete function will be called when the [query](Gltf#queries) is ready. Your application Msg type should handle these:

    type Msg
        = GltfMsg Gltf.Msg
        | GltfOnComplete (Result Gltf.Error Gltf.QueryResult)

In the _onComplete_ handler you can use the [content](Gltf#content) functions to get a tree of nodes, animations, cameras etc.

-}
update :
    { toMsg : Msg -> msg
    , onComplete : Result Error QueryResult -> msg
    }
    -> Msg
    -> Gltf
    -> ( Gltf, Cmd msg )
update { toMsg, onComplete } msg model =
    case msg of
        GltfLoaded query (Ok gltf) ->
            let
                bufferStore : BufferStore
                bufferStore =
                    BufferStore.init gltf
            in
            if BufferStore.isComplete bufferStore then
                let
                    queryResult : Result Error QueryResult
                    queryResult =
                        runQuery gltf TextureStore.init bufferStore query
                in
                ( Progress
                    { gltf = gltf
                    , query = query
                    , bufferStore = bufferStore
                    , textureStore = TextureStore.init
                    }
                , case queryResult of
                    Ok queryResult_ ->
                        case loadTextures queryResult_ of
                            Just cmd ->
                                cmd |> Cmd.map (GltfProgressMsg >> toMsg)

                            Nothing ->
                                Ok queryResult_ |> Task.succeed |> Task.perform onComplete

                    Err error ->
                        Err error |> Task.succeed |> Task.perform onComplete
                )

            else
                ( Progress
                    { gltf = gltf
                    , query = query
                    , bufferStore = bufferStore
                    , textureStore = TextureStore.init
                    }
                , loadBuffers gltf bufferStore |> Cmd.map (GltfProgressMsg >> toMsg)
                )

        GltfLoaded _ (Err error) ->
            ( model
            , Err error |> Task.succeed |> Task.perform onComplete
            )

        GltfProgressMsg progressMsg ->
            case model of
                Initializing ->
                    ( model
                    , Cmd.none
                    )

                Progress model_ ->
                    updateProgress (GltfProgressMsg >> toMsg) onComplete progressMsg model_
                        |> Tuple.mapFirst Progress


updateProgress :
    (ProgressMsg -> msg)
    -> (Result Error QueryResult -> msg)
    -> ProgressMsg
    -> Model
    -> ( Model, Cmd msg )
updateProgress toMsg onComplete msg model =
    case msg of
        BufferLoaded (Ok ( index, bytes )) ->
            let
                bufferStore_ : BufferStore
                bufferStore_ =
                    BufferStore.insert index bytes model.bufferStore
            in
            if BufferStore.isComplete bufferStore_ then
                let
                    queryResult : Result Error QueryResult
                    queryResult =
                        runQuery model.gltf model.textureStore bufferStore_ model.query
                in
                ( { model | bufferStore = bufferStore_ }
                , case queryResult of
                    Ok queryResult_ ->
                        case loadTextures queryResult_ of
                            Just cmd ->
                                cmd |> Cmd.map toMsg

                            Nothing ->
                                Ok queryResult_ |> Task.succeed |> Task.perform onComplete

                    Err error ->
                        Err error |> Task.succeed |> Task.perform onComplete
                )

            else
                ( { model | bufferStore = bufferStore_ }
                , Cmd.none
                )

        BufferLoaded (Err error) ->
            ( model
            , Err error |> Task.succeed |> Task.perform onComplete
            )

        TextureLoaded textureIndex result ->
            case result of
                Ok texture ->
                    let
                        textureStore_ : TextureStore
                        textureStore_ =
                            TextureStore.insert textureIndex texture model.textureStore
                    in
                    ( { model | textureStore = textureStore_ }
                    , if TextureStore.isComplete textureStore_ then
                        let
                            queryResult : Result Error QueryResult
                            queryResult =
                                runQuery model.gltf textureStore_ model.bufferStore model.query
                        in
                        queryResult |> Task.succeed |> Task.perform onComplete

                      else
                        Cmd.none
                    )

                Err error ->
                    ( model
                    , error |> TextureError |> Err |> Task.succeed |> Task.perform onComplete
                    )

        LoadTexture { textureIndex, image, maybeSampler } ->
            case TextureStore.get textureIndex model.textureStore of
                Just _ ->
                    ( model
                    , Cmd.none
                    )

                Nothing ->
                    case Gltf.Query.Task.loadTextureTask model.gltf model.bufferStore image maybeSampler of
                        Just task ->
                            ( { model | textureStore = TextureStore.insertLoading textureIndex model.textureStore }
                            , Task.attempt (TextureLoaded textureIndex) task |> Cmd.map toMsg
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )


loadBuffers : Internal.Gltf.Gltf -> BufferStore -> Cmd ProgressMsg
loadBuffers gltf bufferStore =
    let
        bufferUrls : List ( Int, { byteLength : Int, uri : String } )
        bufferUrls =
            BufferStore.getItemsToLoad gltf bufferStore

        loadBufferCmds : List (Cmd ProgressMsg)
        loadBufferCmds =
            bufferUrls
                |> List.map
                    (\( index, { byteLength, uri } ) ->
                        Http.get
                            { url = uri
                            , expect =
                                Http.expectBytes
                                    (\result ->
                                        case result of
                                            Ok bytes ->
                                                BufferLoaded (Ok ( index, bytes ))

                                            Err error ->
                                                BufferLoaded (Err (HttpError error))
                                    )
                                    (Bytes.Decode.bytes byteLength)
                            }
                    )
    in
    Cmd.batch loadBufferCmds


loadTextures : QueryResult -> Maybe (Cmd ProgressMsg)
loadTextures (QueryResult _ gltf _ textureStore trees) =
    let
        textureSourceCmd : Mesh -> List (Cmd ProgressMsg)
        textureSourceCmd mesh =
            case MeshHelper.toMaterial mesh of
                Just (Gltf.Material.Material m) ->
                    let
                        maybeLoadTextureInfo : Maybe Gltf.Material.TextureIndex -> Maybe LoadTextureInfo
                        maybeLoadTextureInfo maybeTextureIndex =
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
                                                                    (\image ->
                                                                        { textureIndex = textureIndex
                                                                        , image = image
                                                                        , maybeSampler =
                                                                            textureIndex
                                                                                |> TextureIndex.toSamplerIndex
                                                                                |> Maybe.andThen (Common.samplerAtIndex gltf)
                                                                        }
                                                                    )
                                                        )
                                   )

                        loadTexture : LoadTextureInfo -> Cmd ProgressMsg
                        loadTexture effect =
                            Task.perform LoadTexture (Task.succeed effect)
                    in
                    [ maybeLoadTextureInfo m.pbrMetallicRoughness.baseColorTexture |> Maybe.map loadTexture
                    , maybeLoadTextureInfo m.pbrMetallicRoughness.metallicRoughnessTexture |> Maybe.map loadTexture
                    , maybeLoadTextureInfo m.normalTexture |> Maybe.map loadTexture
                    , maybeLoadTextureInfo m.occlusionTexture |> Maybe.map loadTexture
                    , maybeLoadTextureInfo m.emissiveTexture |> Maybe.map loadTexture
                    ]
                        |> List.filterMap identity

                Nothing ->
                    []

        doTree : Tree Node -> List (Cmd ProgressMsg)
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



--------------------------------------------------- Load content


{-| Get default scene from a file of type **.glb**

**Note:** If file does not specify a default scene this returns the first in list

-}
getBinary : String -> (Msg -> msg) -> Cmd msg
getBinary url msg =
    getBinaryWithQuery url DefaultSceneQuery msg


{-| Get default scene from a file of type **.gltf**

**Note:** If file does not specify a default scene this returns the first in list

-}
getGltf : String -> (Msg -> msg) -> Cmd msg
getGltf url msg =
    getGltfWithQuery url DefaultSceneQuery msg


{-| Get content from a file of type **.glb** by supplying one of following queries:

  - [Gltf.defaultSceneQuery](Gltf#defaultSceneQuery) (same as using [getBinary](Gltf#getBinary))
  - [Gltf.sceneQuery](Gltf#sceneQuery)

-}
getBinaryWithQuery :
    String
    -> Query
    -> (Msg -> msg)
    -> Cmd msg
getBinaryWithQuery url query toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectBytes
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> GltfLoaded query
                        |> toMsg
                )
                (Internal.Gltf.bytesDecoder url)
        }


{-| Get content from a file of type **.gltf** by supplying one of following queries:

  - [Gltf.defaultSceneQuery](Gltf#defaultSceneQuery) (same as using [getGltf](Gltf#getGltf))
  - [Gltf.sceneQuery](Gltf#sceneQuery)

-}
getGltfWithQuery :
    String
    -> Query
    -> (Msg -> msg)
    -> Cmd msg
getGltfWithQuery url query toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> GltfLoaded query
                        |> toMsg
                )
                (Internal.Gltf.decoder url)
        }



--------------------------------------------------- Queries


{-| Queries the glTF file for the default scene. If no default scene is defined this will query the first scene.
-}
defaultSceneQuery : Query
defaultSceneQuery =
    DefaultSceneQuery


{-| Queries the glTF file for scene by index.
-}
sceneQuery : Int -> Query
sceneQuery index =
    SceneQuery index


runQuery : Internal.Gltf.Gltf -> TextureStore -> BufferStore -> Query -> Result Error QueryResult
runQuery gltf textureStore bufferStore query =
    case query of
        DefaultSceneQuery ->
            sceneAtIndex2 textureStore bufferStore DefaultSceneQuery gltf.scene gltf

        SceneQuery index ->
            sceneAtIndex2 textureStore bufferStore (SceneQuery index) (Scene.Index index) gltf


sceneAtIndex2 : TextureStore -> BufferStore -> Query -> Scene.Index -> Internal.Gltf.Gltf -> Result Error QueryResult
sceneAtIndex2 textureStore bufferStore query index gltf =
    Common.sceneAtIndex gltf index
        |> Maybe.map
            (\(Scene scene) ->
                scene.nodes
                    |> List.filterMap
                        (\(Internal.Node.Index nodeIndex) -> nodeTree nodeIndex gltf |> Result.toMaybe)
                    |> List.map (Tree.map (nodeFromNode gltf bufferStore))
                    |> QueryResult query gltf bufferStore textureStore
            )
        |> Result.fromMaybe SceneNotFound



--------------------------------------------------- Content


{-| Get all animations
-}
animations : QueryResult -> List Animation
animations (QueryResult _ gltf bufferStore _ _) =
    AnimationHelper.extractAnimations gltf bufferStore


{-| Get all cameras
-}
cameras : QueryResult -> List Camera
cameras (QueryResult _ gltf _ _ _) =
    gltf.cameras |> Array.toList


{-| Get all skins
-}
skins : QueryResult -> List Skin
skins (QueryResult _ gltf bufferStore _ _) =
    gltf.skins
        |> Array.toIndexedList
        |> List.map (Tuple.first >> Gltf.Skin.Index)
        |> List.filterMap (SkinHelper.skinAtIndex gltf bufferStore)


{-| Get node trees returned by [query](Gltf#queries)
-}
nodeTrees : QueryResult -> List (Tree Node)
nodeTrees (QueryResult _ _ _ _ nodes) =
    nodes


{-| Get camera by index
-}
cameraByIndex : Gltf.Camera.Index -> QueryResult -> Maybe Camera
cameraByIndex (Gltf.Camera.Index index) (QueryResult _ gltf _ _ _) =
    Array.get index gltf.cameras


{-| Get texture by index.
-}
textureWithIndex : QueryResult -> Gltf.Material.TextureIndex -> Maybe WebGL.Texture.Texture
textureWithIndex (QueryResult _ _ _ textureStore _) textureIndex =
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


nodeFromNode : Internal.Gltf.Gltf -> BufferStore -> Internal.Node.Node -> Node
nodeFromNode gltf bufferStore node =
    case node |> (\(Internal.Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Gltf.Node.SkinnedMeshNode (triangularMeshesFromNode gltf bufferStore node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Internal.Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> Gltf.Node.CameraNode cameraIndex

                Nothing ->
                    case triangularMeshesFromNode gltf bufferStore node of
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


triangularMeshesFromNode : Internal.Gltf.Gltf -> BufferStore -> Internal.Node.Node -> Maybe (List Mesh)
triangularMeshesFromNode gltf bufferStore (Internal.Node.Node node) =
    node.meshIndex
        |> Maybe.andThen (Common.meshAtIndex gltf)
        |> Maybe.map
            (\{ primitives } ->
                primitives |> List.map (MeshHelper.fromPrimitive gltf bufferStore)
            )
