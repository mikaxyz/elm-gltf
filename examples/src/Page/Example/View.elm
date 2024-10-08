module Page.Example.View exposing (view)

import Gltf
import Gltf.Animation exposing (Animation)
import Gltf.Camera
import Gltf.Scene
import Html exposing (Html, a, aside, div, fieldset, h1, label, legend, option, progress, select, span, text)
import Html.Attributes as HA exposing (class, href, style, value)
import Html.Events
import Json.Decode as JD
import Math.Vector3 exposing (vec3)
import Page.Example.Material as Material
import Page.Example.Model as Model exposing (Model, Msg(..))
import Page.Example.PbrMaterial
import Page.Example.Scene as Scene
import RemoteData exposing (RemoteData)
import Tree
import WebGL
import WebGL.Texture
import XYZMika.XYZ
import XYZMika.XYZ.Material
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene exposing (Scene)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)
import Xyz.Mika.Dragon as Dragon


view : Model -> Html Msg
view model =
    let
        materialConfig : RemoteData Model.Error Page.Example.PbrMaterial.Config
        materialConfig =
            RemoteData.map Page.Example.PbrMaterial.Config
                model.environmentTexture
                |> RemoteData.andMap model.specularEnvironmentTexture
                |> RemoteData.andMap model.brdfLUTTexture

        data =
            RemoteData.map
                (\queryResult scene fallbackTexture config ->
                    { gltfQueryResult = queryResult
                    , scene = scene
                    , fallbackTexture = fallbackTexture
                    , config = config
                    }
                )
                model.queryResult
                |> RemoteData.andMap model.scene
                |> RemoteData.andMap model.fallbackTexture
                |> RemoteData.andMap materialConfig
    in
    case data of
        RemoteData.NotAsked ->
            progressIndicatorView "Loading"

        RemoteData.Loading ->
            progressIndicatorView "Loading"

        RemoteData.Failure error ->
            h1 [] [ text <| Debug.toString error ]

        RemoteData.Success { gltfQueryResult, scene, fallbackTexture, config } ->
            div [ style "display" "contents" ]
                [ sceneOptionsView model gltfQueryResult
                , sceneView model
                    gltfQueryResult
                    model.activeAnimation
                    scene
                    fallbackTexture
                    config
                , model.attribution |> Maybe.map attributionView |> Maybe.withDefault (text "")
                ]


attributionView : Model.Attribution -> Html msg
attributionView _ =
    Html.aside [ class "attribution" ]
        [ a [ href "https://poly.pizza/m/xqEzosAVYX" ] [ Html.text "Zombie" ]
        , Html.text " by "
        , a [ href "https://poly.pizza/u/bachosoftdesign" ] [ Html.text "bachosoftdesign" ]
        , Html.text " ["
        , a [ href "https://creativecommons.org/licenses/by/3.0/" ] [ Html.text "CC - BY" ]
        , Html.text "] via "
        , Html.a [ href "https://poly.pizza" ] [ text "Poly Pizza" ]
        ]


progressIndicatorView : String -> Html msg
progressIndicatorView message =
    label [ class "progress-bar" ]
        [ span [ class "progress-bar__message" ] [ text message ]
        , progress [ class "progress-bar__indicator" ] []
        ]


sceneOptionsView : Model -> Gltf.QueryResult -> Html Msg
sceneOptionsView model gltfQueryResult =
    aside [ class "options" ]
        [ case
            Gltf.cameras gltfQueryResult
                |> List.map
                    (\camera ->
                        { name = camera.name
                        , index = camera.index |> (\(Gltf.Camera.Index index) -> index) |> Just
                        }
                    )
          of
            [] ->
                text ""

            cameras ->
                fieldset [ class "options__title" ]
                    [ legend [ class "options__title" ] [ text "Camera" ]
                    , { name = Just "Interactive", index = Nothing }
                        :: cameras
                        |> List.map
                            (\camera ->
                                option
                                    [ camera.index |> Maybe.map String.fromInt |> Maybe.withDefault "default" |> value ]
                                    [ text (camera.name |> Maybe.withDefault ("Camera " ++ String.fromInt (Maybe.withDefault -1 camera.index)))
                                    ]
                            )
                        |> select
                            [ class "options__camera-select"
                            , onChange (String.toInt >> Maybe.map Gltf.Camera.Index >> UserSelectedCamera)
                            ]
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
                                    (\index animation ->
                                        { name = Gltf.Animation.name animation
                                        , index = Just index
                                        , selected = model.activeAnimation == Just animation
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
                        |> select
                            [ class "options__animation-select"
                            , onChange (String.toInt >> UserSelectedAnimation)
                            ]
                    ]
        , case Gltf.scenes gltfQueryResult of
            _ :: [] ->
                text ""

            scenes ->
                fieldset [ class "options__title" ]
                    [ legend [ class "options__title" ] [ text "Animation" ]
                    , (scenes
                        |> List.map
                            (\scene ->
                                { name = scene.name
                                , index = scene.index |> (\(Gltf.Scene.Index index) -> Just index)
                                , selected =
                                    model.activeScene
                                        |> Maybe.map (\index -> index == scene.index)
                                        |> Maybe.withDefault scene.default
                                }
                            )
                      )
                        |> List.map
                            (\{ name, index, selected } ->
                                option
                                    [ index |> Maybe.map String.fromInt |> Maybe.withDefault "default" |> value
                                    , HA.selected selected
                                    ]
                                    [ text (name |> Maybe.withDefault ("Scene " ++ (index |> Maybe.map String.fromInt |> Maybe.withDefault "-1")))
                                    ]
                            )
                        |> select
                            [ class "options__scene-select"
                            , onChange (String.toInt >> Maybe.withDefault 0 >> Gltf.Scene.Index >> UserSelectedScene)
                            ]
                    ]
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change" (Html.Events.targetValue |> JD.map tagger)


renderer :
    WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Gltf.QueryResult
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
    -> Gltf.QueryResult
    -> Maybe Animation
    -> Scene Scene.ObjectId Material.Name
    -> WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Html Msg
sceneView model gltfQueryResult animation scene fallbackTexture config =
    XYZMika.XYZ.view
        model.viewport
        (renderer fallbackTexture config gltfQueryResult)
        |> XYZMika.XYZ.withDefaultLights [ Light.directional (vec3 -1 1 1) ]
        |> XYZMika.XYZ.withModifiers (Scene.modifiers model.time animation (Gltf.skins gltfQueryResult))
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
            (HA.id "viewport"
                :: Dragon.dragEvents DragonMsg
            )
            scene
