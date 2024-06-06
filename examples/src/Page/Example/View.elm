module Page.Example.View exposing (view)

import Gltf exposing (Gltf)
import Html exposing (Html, h1, text)
import Html.Attributes as HA
import Material
import Math.Vector3 exposing (vec3)
import Page.Example.Model exposing (Model, Msg(..))
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
    case RemoteData.map2 Tuple.pair model.gltf model.scene of
        RemoteData.NotAsked ->
            h1 [] [ text <| "ERROR" ]

        RemoteData.Loading ->
            h1 [] [ text <| "Loading" ]

        RemoteData.Failure _ ->
            h1 [] [ text <| "Error" ]

        RemoteData.Success ( gltf, scene ) ->
            model.fallbackTexture
                |> RemoteData.toMaybe
                |> Maybe.map (sceneView model gltf scene)
                |> Maybe.withDefault (h1 [] [ text <| "Loading fallbackTexture" ])


renderer :
    WebGL.Texture.Texture
    -> Maybe Material.Name
    -> XYZMika.XYZ.Material.Options
    -> Uniforms u
    -> Object a Material.Name
    -> WebGL.Entity
renderer fallbackTexture name =
    case name of
        Just materialName ->
            Material.renderer fallbackTexture materialName

        Nothing ->
            XYZMika.XYZ.Material.Simple.renderer


sceneView : Model -> Gltf -> Scene Scene.ObjectId Material.Name -> WebGL.Texture.Texture -> Html Msg
sceneView model gltf scene fallbackTexture =
    XYZMika.XYZ.view
        model.viewport
        (renderer fallbackTexture)
        |> XYZMika.XYZ.withDefaultLights [ Light.directional (vec3 -1 1 1) ]
        |> XYZMika.XYZ.withModifiers (Scene.modifiers model.time model.animations gltf)
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
