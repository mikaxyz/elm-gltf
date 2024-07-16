module Page.Example.View exposing (view)

import Array
import Gltf exposing (Gltf)
import Gltf.Query
import Gltf.Query.Animation as Animation
import Gltf.Query.Camera
import Html exposing (Html, aside, div, fieldset, h1, label, legend, option, progress, select, span, text)
import Html.Attributes as HA exposing (class, style, value)
import Html.Events
import Json.Decode as JD
import Material
import Math.Vector3 exposing (vec3)
import Page.Example.Model exposing (Model, Msg(..))
import Page.Example.PbrMaterial
import Page.Example.Scene as Scene
import RemoteData exposing (RemoteData)
import Tree
import WebGL
import WebGL.Texture
import XYZMika.Dragon as Dragon
import XYZMika.XYZ
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene exposing (Scene)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


view : Model -> Html Msg
view model =
    let
        materialConfig : RemoteData Page.Example.Model.Error Page.Example.PbrMaterial.Config
        materialConfig =
            RemoteData.map Page.Example.PbrMaterial.Config
                model.environmentTexture
                |> RemoteData.andMap model.specularEnvironmentTexture
                |> RemoteData.andMap model.brdfLUTTexture

        data =
            RemoteData.map
                (\gltf scene fallbackTexture config ->
                    { gltf = gltf
                    , scene = scene
                    , fallbackTexture = fallbackTexture
                    , config = config
                    }
                )
                model.gltf
                |> RemoteData.andMap model.scene
                |> RemoteData.andMap model.fallbackTexture
                |> RemoteData.andMap materialConfig
    in
    case data of
        RemoteData.NotAsked ->
            progressIndicatorView "Loading"

        RemoteData.Loading ->
            progressIndicatorView "Loading"

        RemoteData.Failure _ ->
            h1 [] [ text <| "Error" ]

        RemoteData.Success { gltf, scene, fallbackTexture, config } ->
            case model.queryResult of
                Just gltfQueryResult ->
                    div [ style "display" "contents" ]
                        [ sceneOptionsView gltf
                            model
                        , sceneView model
                            gltfQueryResult
                            model.activeAnimation
                            gltf
                            scene
                            fallbackTexture
                            config
                        ]

                Nothing ->
                    progressIndicatorView "Loading"


progressIndicatorView : String -> Html msg
progressIndicatorView message =
    label [ class "progress-bar" ]
        [ span [ class "progress-bar__message" ] [ text message ]
        , progress [ class "progress-bar__indicator" ] []
        ]


sceneOptionsView : Gltf -> Model -> Html Msg
sceneOptionsView gltf model =
    aside [ class "options" ]
        [ case
            gltf.cameras
                |> Array.toList
                |> List.map
                    (\camera ->
                        { name = camera.name
                        , index = camera.index |> (\(Gltf.Query.Camera.Index index) -> index) |> Just
                        }
                    )
          of
            [] ->
                text ""

            cameras ->
                fieldset [ class "options__title" ]
                    [ legend [ class "options__title" ] [ text "Camera" ]
                    , { name = Just "Select", index = Nothing }
                        :: cameras
                        |> List.map
                            (\camera ->
                                option
                                    [ camera.index |> Maybe.map String.fromInt |> Maybe.withDefault "default" |> value ]
                                    [ text (camera.name |> Maybe.withDefault ("Camera " ++ String.fromInt (Maybe.withDefault -1 camera.index)))
                                    ]
                            )
                        |> select [ onChange (String.toInt >> Maybe.map Gltf.Query.Camera.Index >> UserSelectedCamera) ]
                    ]
        , case model.animations of
            [] ->
                text ""

            animations ->
                fieldset [ class "options__title" ]
                    [ legend [ class "options__title" ] [ text "Animation" ]
                    , { name = Just "Disabled", index = Nothing, selected = model.activeAnimation == Nothing }
                        :: (animations
                                |> List.indexedMap
                                    (\index (Animation.ExtractedAnimation animation) ->
                                        { name = animation.name
                                        , index = Just index
                                        , selected = model.activeAnimation == Just (Animation.ExtractedAnimation animation)
                                        }
                                    )
                           )
                        |> List.map
                            (\{ name, index, selected } ->
                                option
                                    [ index |> Maybe.map String.fromInt |> Maybe.withDefault "default" |> value
                                    , HA.selected selected
                                    ]
                                    [ text (name |> Maybe.withDefault ("Animation " ++ (index |> Maybe.map String.fromInt |> Maybe.withDefault "-1")))
                                    ]
                            )
                        |> select [ onChange (String.toInt >> UserSelectedAnimation) ]
                    ]
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change" (Html.Events.targetValue |> JD.map tagger)


renderer :
    WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Gltf.Query.QueryResult
    -> Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Uniforms u
    -> Object a Material.Name
    -> WebGL.Entity
renderer fallbackTexture textures gltfQueryResult name =
    case name of
        Just materialName ->
            Material.renderer fallbackTexture textures gltfQueryResult materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer


sceneView :
    Model
    -> Gltf.Query.QueryResult
    -> Maybe Animation.ExtractedAnimation
    -> Gltf
    -> Scene Scene.ObjectId Material.Name
    -> WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Html Msg
sceneView model gltfQueryResult animation gltf scene fallbackTexture config =
    XYZMika.XYZ.view
        model.viewport
        (renderer fallbackTexture config gltfQueryResult)
        |> XYZMika.XYZ.withDefaultLights [ Light.directional (vec3 -1 1 1) ]
        |> XYZMika.XYZ.withModifiers (Scene.modifiers model.time animation gltf)
        |> XYZMika.XYZ.withSceneOptions model.sceneOptions
        |> XYZMika.XYZ.withRenderOptions
            (\graph ->
                let
                    index : Int
                    index =
                        Tuple.first (Tree.label graph)
                in
                if model.selectedTreeIndex == Just index then
                    Just { showBoundingBox = True }

                else
                    Nothing
            )
        |> XYZMika.XYZ.toHtml
            [ HA.id "viewport"
            , Dragon.dragEvents DragonMsg
            ]
            scene
