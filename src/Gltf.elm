module Gltf exposing
    ( Asset, Error(..), Query, QueryResult
    , Msg, Gltf, init, update
    , getBinary, getGltf, getBinaryWithQuery, getGltfWithQuery
    , defaultSceneQuery, sceneQuery, query
    , animations, cameras, nodeTrees, scenes, skins, cameraByIndex, textureWithIndex
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format


# Types

@docs Asset, Error, Query, QueryResult


# Wiring

@docs Msg, Gltf, init, update


# Load content

@docs getBinary, getGltf, getBinaryWithQuery, getGltfWithQuery


# Queries

@docs defaultSceneQuery, sceneQuery, query


# Content

@docs animations, cameras, nodeTrees, scenes, skins, cameraByIndex, textureWithIndex

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
import Gltf.Scene exposing (Scene)
import Gltf.Skin exposing (Skin)
import Gltf.Transform
import Http
import Internal.Gltf
import Internal.Image
import Internal.Node
import Internal.Sampler
import Internal.Scene
import Internal.Skin
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
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
    = QueryResult Query Internal.Gltf.Gltf BufferStore TextureStore (List Skin) (List (Tree Node))



--------------------------------------------------- Wiring


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
        GltfLoaded query_ (Ok gltf) ->
            let
                bufferStore : BufferStore
                bufferStore =
                    BufferStore.init gltf
            in
            if BufferStore.isComplete bufferStore then
                let
                    queryResult : Result Error QueryResult
                    queryResult =
                        runQuery gltf TextureStore.init bufferStore query_
                in
                ( Progress
                    { gltf = gltf
                    , query = query_
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
                    , query = query_
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
loadTextures (QueryResult _ gltf _ textureStore _ trees) =
    let
        textureSourceCmd : Mesh -> List (Cmd ProgressMsg)
        textureSourceCmd mesh =
            case MeshHelper.toMaterial mesh of
                Just (Gltf.Material.Material m) ->
                    let
                        maybeLoadTextureInfo : Gltf.Material.Texture -> Maybe LoadTextureInfo
                        maybeLoadTextureInfo (Gltf.Material.Texture { index }) =
                            case TextureStore.get index textureStore of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    (index |> TextureIndex.toImageIndex |> Common.imageAtIndex gltf)
                                        |> Maybe.map
                                            (\image ->
                                                { textureIndex = index
                                                , image = image
                                                , maybeSampler =
                                                    index
                                                        |> TextureIndex.toSamplerIndex
                                                        |> Maybe.andThen (Common.samplerAtIndex gltf)
                                                }
                                            )

                        loadTexture : LoadTextureInfo -> Cmd ProgressMsg
                        loadTexture effect =
                            Task.perform LoadTexture (Task.succeed effect)
                    in
                    [ m.pbrMetallicRoughness.baseColorTexture
                        |> Maybe.andThen maybeLoadTextureInfo
                        |> Maybe.map loadTexture
                    , m.pbrMetallicRoughness.metallicRoughnessTexture
                        |> Maybe.andThen maybeLoadTextureInfo
                        |> Maybe.map loadTexture
                    , m.normalTexture
                        |> Maybe.andThen maybeLoadTextureInfo
                        |> Maybe.map loadTexture
                    , m.occlusionTexture
                        |> Maybe.andThen maybeLoadTextureInfo
                        |> Maybe.map loadTexture
                    , m.emissiveTexture
                        |> Maybe.andThen maybeLoadTextureInfo
                        |> Maybe.map loadTexture
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
getBinaryWithQuery url query_ toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectBytes
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> GltfLoaded query_
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
getGltfWithQuery url query_ toMsg =
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (\result ->
                    result
                        |> Result.mapError HttpError
                        |> GltfLoaded query_
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


{-| Query an already loaded file
-}
query : Query -> (Msg -> msg) -> QueryResult -> Cmd msg
query query_ toMsg (QueryResult _ gltf _ _ _ _) =
    Task.succeed (Ok gltf) |> Task.perform (GltfLoaded query_ >> toMsg)


runQuery : Internal.Gltf.Gltf -> TextureStore -> BufferStore -> Query -> Result Error QueryResult
runQuery gltf textureStore bufferStore query_ =
    case query_ of
        DefaultSceneQuery ->
            sceneAtIndex2 textureStore bufferStore DefaultSceneQuery gltf.scene gltf

        SceneQuery index ->
            sceneAtIndex2 textureStore bufferStore (SceneQuery index) (Internal.Scene.Index index) gltf


sceneAtIndex2 : TextureStore -> BufferStore -> Query -> Internal.Scene.Index -> Internal.Gltf.Gltf -> Result Error QueryResult
sceneAtIndex2 textureStore bufferStore query_ index gltf =
    Common.sceneAtIndex gltf index
        |> Maybe.map
            (\(Internal.Scene.Scene scene) ->
                let
                    skins_ : List Skin
                    skins_ =
                        gltf.skins
                            |> Array.toIndexedList
                            |> List.map (Tuple.first >> Gltf.Skin.Index)
                            |> List.filterMap (SkinHelper.skinAtIndex gltf bufferStore)
                in
                scene.nodes
                    |> List.filterMap
                        (\(Internal.Node.Index nodeIndex) -> nodeTree nodeIndex gltf |> Result.toMaybe)
                    |> List.map (Tree.map (nodeFromNode gltf bufferStore))
                    |> QueryResult query_ gltf bufferStore textureStore skins_
            )
        |> Result.fromMaybe SceneNotFound



--------------------------------------------------- Content


{-| Get all animations
-}
animations : QueryResult -> List Animation
animations (QueryResult _ gltf bufferStore _ _ _) =
    AnimationHelper.extractAnimations gltf bufferStore


{-| Get all cameras
-}
cameras : QueryResult -> List Camera
cameras (QueryResult _ gltf _ _ _ _) =
    gltf.cameras |> Array.toList


{-| Get all skins
-}
skins : QueryResult -> List Skin
skins (QueryResult _ _ _ _ skins_ _) =
    skins_


{-| Get information of all scenes
-}
scenes : QueryResult -> List Scene
scenes (QueryResult _ gltf _ _ _ _) =
    gltf.scenes
        |> Array.toIndexedList
        |> List.map
            (\( i, Internal.Scene.Scene scene ) ->
                { name = scene.name
                , index = Gltf.Scene.Index i
                , default = gltf.scene == Internal.Scene.Index i
                }
            )


{-| Get node trees returned by [query](Gltf#queries)
-}
nodeTrees : QueryResult -> List (Tree Node)
nodeTrees (QueryResult _ _ _ _ _ nodes) =
    nodes


{-| Get camera by index
-}
cameraByIndex : Gltf.Camera.Index -> QueryResult -> Maybe Camera
cameraByIndex (Gltf.Camera.Index index) (QueryResult _ gltf _ _ _ _) =
    Array.get index gltf.cameras


{-| Get texture by index.
-}
textureWithIndex : QueryResult -> Gltf.Material.TextureIndex -> Maybe WebGL.Texture.Texture
textureWithIndex (QueryResult _ _ _ textureStore _ _) textureIndex =
    TextureStore.textureWithTextureIndex textureIndex textureStore



--------------------------------------------------- INTERNAL


nodeIndexFromNode : Internal.Node.Index -> NodeIndex
nodeIndexFromNode (Internal.Node.Index index) =
    NodeIndex index


meshesFromNode : Node -> List Mesh
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


nodeFromNode : Internal.Gltf.Gltf -> BufferStore -> Internal.Node.Node -> Node
nodeFromNode gltf bufferStore node =
    let
        jointNodeSkinId : Internal.Node.Node -> Maybe ( Gltf.Skin.Index, Maybe Float )
        jointNodeSkinId (Internal.Node.Node node_) =
            let
                (Internal.Node.Index nodeIndex) =
                    node_.index

                id : Maybe Gltf.Skin.Index
                id =
                    gltf.skins
                        |> Array.indexedMap
                            (\index skin ->
                                if List.member (Internal.Skin.JointNodeIndex nodeIndex) skin.joints then
                                    Just (Gltf.Skin.Index index)

                                else
                                    Nothing
                            )
                        |> Array.toList
                        |> List.filterMap identity
                        |> List.head
            in
            case id of
                Just skinId ->
                    let
                        length : Gltf.Transform.Transform -> Maybe Float
                        length transform =
                            case transform of
                                Gltf.Transform.TRS { translation } ->
                                    translation
                                        |> Maybe.map Vec3.length

                                Gltf.Transform.Matrix mat ->
                                    Vec3.vec3 0 0 0
                                        |> Mat4.transform mat
                                        |> Vec3.length
                                        |> Just
                    in
                    Just
                        ( skinId
                        , node_.children
                            |> List.head
                            |> Maybe.andThen (Common.nodeAtIndex gltf)
                            |> Maybe.andThen (\(Internal.Node.Node childNode) -> length childNode.transform)
                        )

                Nothing ->
                    Nothing
    in
    case node |> (\(Internal.Node.Node { skinIndex }) -> skinIndex) |> Maybe.map (\(Internal.Skin.Index index) -> Gltf.Skin.Index index) of
        Just skinIndex ->
            node
                |> propertiesFromNode
                |> Gltf.Node.SkinnedMesh (triangularMeshesFromNode gltf bufferStore node |> Maybe.withDefault []) skinIndex

        Nothing ->
            case node |> (\(Internal.Node.Node x) -> x.cameraIndex) of
                Just cameraIndex ->
                    node
                        |> propertiesFromNode
                        |> Gltf.Node.Camera cameraIndex

                Nothing ->
                    case jointNodeSkinId node of
                        Just ( skinIndex, length ) ->
                            node
                                |> propertiesFromNode
                                |> Gltf.Node.Bone skinIndex length

                        Nothing ->
                            case triangularMeshesFromNode gltf bufferStore node of
                                Just meshes ->
                                    node
                                        |> propertiesFromNode
                                        |> Gltf.Node.Mesh meshes

                                Nothing ->
                                    node
                                        |> propertiesFromNode
                                        |> Gltf.Node.Empty


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
