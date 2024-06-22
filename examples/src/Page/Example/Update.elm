module Page.Example.Update exposing (update)

import Array
import Browser.Dom
import Color
import Gltf.Query as Query
import Gltf.Query.Animation as Animation
import Internal.Camera
import Internal.Node
import Internal.Scene
import Keyboard
import Material
import Math.Vector2 as Vec2 exposing (vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Page.Example.Model as Model exposing (Model, Msg(..))
import Page.Example.Scene as Scene
import RemoteData exposing (RemoteData)
import Task
import Tree
import XYZMika.Dragon as Dragon
import XYZMika.XYZ.Scene as XYZScene
import XYZMika.XYZ.Scene.Camera as XYZCamera
import XYZMika.XYZ.Scene.Light as XYZLight
import XYZMika.XYZ.Scene.Object as XYZObject
import XYZMika.XYZ.Scene.Options as XYZSceneOptions
import XYZMika.XYZ.Scene.Util as XYZUtil


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrameDelta delta ->
            ( if model.paused then
                model

              else
                { model | time = model.time + (delta / 1000) }
            , Cmd.none
            )

        OnResize ->
            ( model
            , getViewPort
            )

        OnViewportElement result ->
            ( result
                |> Result.map
                    (\element ->
                        { model
                            | viewport = Model.viewportFromDomElement element
                            , viewPortElement = Just element
                        }
                    )
                |> Result.withDefault model
            , Cmd.none
            )

        DragonMsg msg_ ->
            Dragon.update
                { tagger = DragonMsg
                , onDragUpdate = DragonOnDrag
                , onMouseUp = OnMouseUp
                }
                msg_
                model.dragon
                |> Tuple.mapFirst (\dragon -> { model | dragon = dragon })

        DragonOnDrag drag ->
            let
                sensitivity =
                    model.sceneSize
            in
            ( { model
                | scene =
                    case Model.dragTarget model of
                        Model.CameraOrbit ->
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withCameraMap
                                        (\camera ->
                                            camera
                                                --|> XYZCamera.withOrbitY -(drag.x / 40 * sensitivity)
                                                |> XYZCamera.withOrbitY -(drag.x / 100)
                                                |> XYZCamera.withPositionMap (\position -> Vec3.setY (Vec3.getY position + (drag.y / 80 * sensitivity)) position)
                                        )
                                    )

                        Model.CameraPan ->
                            model.scene |> RemoteData.map (XYZScene.withCameraMap (XYZCamera.withPan (Vec2.scale (sensitivity / 100) (vec2 drag.x drag.y))))

                        Model.CameraZoom ->
                            model.scene |> RemoteData.map (XYZScene.withCameraMap (XYZCamera.withZoom (drag.y / 20 * sensitivity)))

                        Model.Default ->
                            let
                                move : Vec3
                                move =
                                    model.scene
                                        |> RemoteData.map (XYZScene.camera >> XYZCamera.inPlane (vec2 drag.x drag.y))
                                        |> RemoteData.withDefault (vec3 0 0 0)
                                        |> Vec3.scale 0.01
                            in
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.map
                                        (Tree.indexedMap
                                            (\index object ->
                                                if model.selectedTreeIndex == Just index then
                                                    XYZObject.map
                                                        (\data -> { data | position = Vec3.add move data.position })
                                                        object

                                                else
                                                    object
                                            )
                                        )
                                    )
              }
                |> setSceneSize
            , Cmd.none
            )

        OnMouseUp pos ->
            let
                selectedTreeIndexAtClickPosition :
                    XYZScene.Scene Scene.ObjectId Material.Name
                    -> Browser.Dom.Element
                    -> Maybe Int
                selectedTreeIndexAtClickPosition scene viewPortElement =
                    XYZUtil.selectGraphAtClickPosition
                        { viewport =
                            { width = toFloat model.viewport.width
                            , height = toFloat model.viewport.height
                            }
                        , viewPortElement = viewPortElement
                        }
                        (model.gltf |> RemoteData.map (Scene.modifiers model.time model.animations) |> RemoteData.withDefault [])
                        scene
                        ( pos.x, pos.y )
                        |> Maybe.map Tuple.first
            in
            ( { model
                | selectedTreeIndex =
                    if Model.dragTarget model == Model.Default then
                        Maybe.map2 Tuple.pair (RemoteData.toMaybe model.scene) model.viewPortElement
                            |> Maybe.andThen
                                (\( scene, viewPortElement ) ->
                                    selectedTreeIndexAtClickPosition scene viewPortElement
                                )

                    else
                        model.selectedTreeIndex
              }
            , Cmd.none
            )

        KeyboardMsg msg_ ->
            ( { model | keyboard = Keyboard.update msg_ model.keyboard }
            , Cmd.none
            )

        OnKeyDown key ->
            let
                lightDistance =
                    6
            in
            case key of
                Keyboard.Alpha 'P' ->
                    ( { model | paused = not model.paused }
                    , Cmd.none
                    )

                Keyboard.Alpha 'X' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 0 -lightDistance 0)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'W' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 0 lightDistance 0)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'D' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 lightDistance 0 0)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'A' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 -lightDistance 0 0)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'E' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 0 0 -lightDistance)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'Z' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 0 0 lightDistance)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 1
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'C' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 -4 2 4)
                                            |> XYZLight.withColor Color.red
                                            |> XYZLight.withIntensity 0.5
                                        , XYZLight.pointLight (vec3 4 2 4)
                                            |> XYZLight.withColor Color.green
                                            |> XYZLight.withIntensity 0.5
                                        , XYZLight.pointLight (vec3 0 2 -4)
                                            |> XYZLight.withColor Color.blue
                                            |> XYZLight.withIntensity 0.5
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'Q' ->
                    ( { model
                        | scene =
                            model.scene
                                |> RemoteData.map
                                    (XYZScene.withLights
                                        [ XYZLight.pointLight (vec3 -4 2 2)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 0.2
                                        , XYZLight.pointLight (vec3 -2 4 0.5)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 0.2
                                        , XYZLight.pointLight (vec3 0 5 0)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 0.2
                                        , XYZLight.pointLight (vec3 2 4 0.5)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 0.2
                                        , XYZLight.pointLight (vec3 4 2 2)
                                            |> XYZLight.withColor Color.white
                                            |> XYZLight.withIntensity 0.2
                                        ]
                                    )
                      }
                    , Cmd.none
                    )

                Keyboard.Alpha 'S' ->
                    ( { model | scene = model.scene |> RemoteData.map XYZScene.withLightsInGraph }
                    , Cmd.none
                    )

                Keyboard.Digit 1 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showGeometryOption)
                    , Cmd.none
                    )

                Keyboard.Digit 2 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showBoundingBoxesOption)
                    , Cmd.none
                    )

                Keyboard.Digit 3 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showBoundingBoxesOverlayOption)
                    , Cmd.none
                    )

                Keyboard.Digit 4 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showLightGizmosOption)
                    , Cmd.none
                    )

                Keyboard.Digit 7 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showGridXOption)
                    , Cmd.none
                    )

                Keyboard.Digit 8 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showGridYOption)
                    , Cmd.none
                    )

                Keyboard.Digit 9 ->
                    ( model |> Model.mapSceneOptions (XYZSceneOptions.toggle XYZSceneOptions.showGridZOption)
                    , Cmd.none
                    )

                Keyboard.Character ',' ->
                    ( model, Cmd.none )

                Keyboard.Character '.' ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FallbackTextureReceived result ->
            case result of
                Ok texture ->
                    ( { model | fallbackTexture = RemoteData.Success texture }
                    , getViewPort
                    )

                Err error ->
                    ( { model | fallbackTexture = RemoteData.Failure (Debug.log "FallbackTextureReceived" error) }
                    , Cmd.none
                    )

        EnvironmentTextureReceived result ->
            case result of
                Ok texture ->
                    ( { model | environmentTexture = RemoteData.Success texture }
                    , getViewPort
                    )

                Err error ->
                    ( { model | environmentTexture = RemoteData.Failure error }
                    , Cmd.none
                    )

        SpecularEnvironmentTextureReceived result ->
            case result of
                Ok texture ->
                    ( { model | specularEnvironmentTexture = RemoteData.Success texture }
                    , getViewPort
                    )

                Err error ->
                    ( { model | specularEnvironmentTexture = RemoteData.Failure error }
                    , Cmd.none
                    )

        BrdfLUTTextureReceived result ->
            case result of
                Ok texture ->
                    ( { model | brdfLUTTexture = RemoteData.Success texture }
                    , getViewPort
                    )

                Err error ->
                    ( { model | brdfLUTTexture = RemoteData.Failure error }
                    , Cmd.none
                    )

        GltfApplyEffect effect ->
            let
                nodes : List (Tree.Tree Query.Node)
                nodes =
                    model.nodes |> List.map (Query.applyEffect effect)
            in
            ( { model
                | nodes = nodes
                , scene =
                    model.scene
                        |> RemoteData.map
                            (XYZScene.map
                                (\_ ->
                                    Scene.graphFromNodes nodes
                                        identity
                                        { camera = default.camera
                                        , projection = default.projection
                                        , sceneSize = model.sceneSize
                                        }
                                )
                            )
              }
            , getViewPort
            )

        GltfReceived result ->
            case result of
                Ok gltf ->
                    let
                        nodes : List (Tree.Tree Query.Node)
                        nodes =
                            gltf
                                |> Query.sceneNodeTrees (Internal.Scene.Index 0)
                                |> Result.withDefault []
                                |> List.map (Tree.map (Query.nodeFromNode gltf))

                        cmds : List (Cmd Msg)
                        cmds =
                            nodes
                                |> List.map (Query.effectsFromNodeTree GltfApplyEffect)

                        scene : XYZScene.Scene Scene.ObjectId Material.Name
                        scene =
                            Scene.initWithNodes
                                nodes
                                identity
                                { camera = default.camera
                                , projection = default.projection
                                , sceneSize = model.sceneSize
                                }
                    in
                    ( { model
                        | gltf = RemoteData.Success gltf
                        , nodes = nodes
                        , animations = Animation.extractAnimations gltf
                        , scene = scene |> RemoteData.Success
                      }
                        |> setSceneSize
                    , getViewPort
                        :: cmds
                        |> Cmd.batch
                    )

                Err error ->
                    ( { model | gltf = RemoteData.Failure error }
                    , Cmd.none
                    )

        UserSelectedCamera Nothing ->
            ( { model
                | activeCamera = Nothing
                , scene =
                    model.scene
                        |> RemoteData.map
                            (XYZScene.unpinCamera >> XYZScene.withPerspectiveProjection default.projection)
              }
            , Cmd.none
            )

        UserSelectedCamera (Just index) ->
            ( { model
                | activeCamera = Just index
                , scene =
                    model.scene
                        |> RemoteData.map
                            (\scene ->
                                let
                                    cameraNodeIndex : Maybe Internal.Node.Index
                                    cameraNodeIndex =
                                        model.nodes
                                            |> List.concatMap
                                                (\tree ->
                                                    Tree.flatten tree
                                                        |> List.map
                                                            (\node ->
                                                                case node of
                                                                    Query.CameraNode cameraIndex (Query.Properties properties) ->
                                                                        if cameraIndex == index then
                                                                            Just properties.nodeIndex

                                                                        else
                                                                            Nothing

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                )
                                            |> List.filterMap identity
                                            |> List.head

                                    maybeGltfCamera : Maybe Internal.Camera.Camera
                                    maybeGltfCamera =
                                        model.gltf
                                            |> RemoteData.toMaybe
                                            |> Maybe.map .cameras
                                            |> Maybe.andThen (Array.get ((\(Internal.Camera.Index i) -> i) index))

                                    applyProjection =
                                        case maybeGltfCamera |> Maybe.map .projection of
                                            Just (Internal.Camera.Perspective p) ->
                                                XYZScene.withPerspectiveProjection
                                                    { fov = 180 / pi * p.yFov
                                                    , near = p.zNear
                                                    , far = p.zFar |> Maybe.withDefault 1000
                                                    }

                                            Just (Internal.Camera.Orthographic p) ->
                                                XYZScene.withOrthographicProjection
                                                    { xMag = p.xMag
                                                    , yMag = p.yMag
                                                    , near = p.zNear
                                                    , far = p.zFar
                                                    }

                                            Nothing ->
                                                identity
                                in
                                scene
                                    |> applyProjection
                                    |> (case cameraNodeIndex of
                                            Just nodeIndex ->
                                                XYZScene.pinCameraToObject (Scene.Camera index nodeIndex)

                                            Nothing ->
                                                XYZScene.withPerspectiveProjection default.projection
                                       )
                            )
              }
            , Cmd.none
            )


default : { camera : XYZCamera.Camera, projection : { fov : number, near : Float, far : number } }
default =
    { camera = XYZCamera.init (vec3 3 8 12) (vec3 0 0 0)
    , projection = { fov = 60, near = 0.1, far = 10000 }
    }


setSceneSize : Model -> Model
setSceneSize model =
    case model.scene |> RemoteData.map XYZScene.camera of
        RemoteData.Success camera ->
            { model | sceneSize = XYZCamera.position camera |> Vec3.distance (XYZCamera.target camera) }

        _ ->
            model


getViewPort : Cmd Msg
getViewPort =
    Browser.Dom.getElement "viewport" |> Task.attempt OnViewportElement
