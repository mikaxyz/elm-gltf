module Page.Example.Material exposing (Name(..), renderer)

import Gltf
import Gltf.Material
import Page.Example.DefaultMaterial
import Page.Example.PbrMaterial
import WebGL exposing (Entity)
import WebGL.Texture
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Name
    = Default
    | PbrMaterial Gltf.Material.Material


renderer :
    WebGL.Texture.Texture
    -> Page.Example.PbrMaterial.Config
    -> Gltf.QueryResult
    -> Name
    -> Material.Options
    -> Uniforms u
    -> Object objectId materialId
    -> Entity
renderer fallbackTexture config gltfQueryResult name =
    case name of
        Default ->
            Page.Example.DefaultMaterial.renderer config

        PbrMaterial (Gltf.Material.Material pbr) ->
            Page.Example.PbrMaterial.renderer config
                { pbrMetallicRoughness =
                    { baseColorTexture =
                        pbr.pbrMetallicRoughness.baseColorTexture
                            |> Maybe.map Gltf.Material.textureIndex
                            |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                            |> Maybe.withDefault fallbackTexture
                    , metallicRoughnessTexture =
                        pbr.pbrMetallicRoughness.metallicRoughnessTexture
                            |> Maybe.map Gltf.Material.textureIndex
                            |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                            |> Maybe.withDefault fallbackTexture
                    }
                , normalTexture =
                    pbr.normalTexture
                        |> Maybe.map Gltf.Material.textureIndex
                        |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                , occlusionTexture =
                    pbr.occlusionTexture
                        |> Maybe.map Gltf.Material.textureIndex
                        |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                , emissiveTexture =
                    pbr.emissiveTexture
                        |> Maybe.map Gltf.Material.textureIndex
                        |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                        |> Maybe.withDefault fallbackTexture
                }
                (Gltf.Material.Material pbr)
