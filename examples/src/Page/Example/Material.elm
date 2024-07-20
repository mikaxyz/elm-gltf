module Page.Example.Material exposing (Name(..), renderer)

import Gltf.Query
import Gltf.Query.Material
import Page.Example.PbrMaterial
import WebGL exposing (Entity)
import WebGL.Texture
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Color
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Name
    = Simple
    | Color
    | Advanced
    | PbrMaterial Gltf.Query.Material.Material


renderer :
    WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Gltf.Query.QueryResult
    -> Name
    -> Material.Options
    -> Uniforms u
    -> Object objectId materialId
    -> Entity
renderer fallbackTexture config gltfQueryResult name =
    case name of
        Simple ->
            XYZMika.XYZ.Material.Simple.renderer

        Color ->
            XYZMika.XYZ.Material.Color.renderer

        Advanced ->
            XYZMika.XYZ.Material.Advanced.renderer

        PbrMaterial (Gltf.Query.Material.Material pbr) ->
            Page.Example.PbrMaterial.renderer config
                { pbrMetallicRoughness =
                    { baseColorTexture =
                        pbr.pbrMetallicRoughness.baseColorTexture
                            |> Maybe.andThen (Gltf.Query.textureWithIndex gltfQueryResult)
                            |> Maybe.withDefault fallbackTexture
                    , metallicRoughnessTexture =
                        pbr.pbrMetallicRoughness.metallicRoughnessTexture
                            |> Maybe.andThen (Gltf.Query.textureWithIndex gltfQueryResult)
                            |> Maybe.withDefault fallbackTexture
                    }
                , normalTexture =
                    pbr.normalTexture
                        |> Maybe.andThen (Gltf.Query.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                , occlusionTexture =
                    pbr.occlusionTexture
                        |> Maybe.andThen (Gltf.Query.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                , emissiveTexture =
                    pbr.emissiveTexture
                        |> Maybe.andThen (Gltf.Query.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                }
                (Gltf.Query.Material.Material pbr)
