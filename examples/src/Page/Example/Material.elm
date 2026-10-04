module Page.Example.Material exposing (Name(..), renderer)

import Gltf
import Gltf.Material
import Gltf.Material.Extensions exposing (Iridescence)
import Gltf.Mesh
import Gltf.Node
import Gltf.Texture exposing (Texture)
import Page.Example.DefaultMaterial
import Page.Example.ErrorMaterial
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
    -> Page.Example.ErrorMaterial.Config
    -> Page.Example.PbrMaterial.Config
    -> Gltf.QueryResult
    -> Name
    -> Material.Options
    -> Uniforms u
    -> Object objectId materialId
    -> Entity
renderer fallbackTexture errorMaterialConfig config gltfQueryResult name =
    case name of
        Default ->
            Page.Example.DefaultMaterial.renderer
                { environmentTexture = config.environmentTexture
                , specularEnvironmentTexture = config.specularEnvironmentTexture
                , brdfLUTTexture = config.brdfLUTTexture
                }

        PbrMaterial (Gltf.Material.Material pbr) ->
            let
                iridescenceTexturesPacked : Result () (Maybe Gltf.Texture.Index)
                iridescenceTexturesPacked =
                    pbr.extensions |> Maybe.andThen .iridescence |> Gltf.Material.Extensions.iridescenceTexturesPackedIndex
            in
            case iridescenceTexturesPacked of
                Ok iridescenceTexturePackedIndex ->
                    Page.Example.PbrMaterial.renderer config
                        { fallbackTexture = fallbackTexture
                        , pbrMetallicRoughness =
                            { baseColorTexture =
                                pbr.pbrMetallicRoughness.baseColorTexture
                                    |> Maybe.map Gltf.Texture.toIndex
                                    |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                    |> Maybe.withDefault fallbackTexture
                            , metallicRoughnessTexture =
                                pbr.pbrMetallicRoughness.metallicRoughnessTexture
                                    |> Maybe.map Gltf.Texture.toIndex
                                    |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                    |> Maybe.withDefault fallbackTexture
                            }
                        , normalTexture =
                            pbr.normalTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , occlusionTexture =
                            pbr.occlusionTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , emissiveTexture =
                            pbr.emissiveTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , transmissionTexture =
                            pbr.extensions
                                |> Maybe.andThen .transmission
                                |> Maybe.andThen .texture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , thicknessTexture =
                            pbr.extensions
                                |> Maybe.andThen .volume
                                |> Maybe.andThen .thicknessTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , anisotropyTexture =
                            pbr.extensions
                                |> Maybe.andThen .anisotropy
                                |> Maybe.andThen .texture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , clearcoatTexture =
                            pbr.extensions
                                |> Maybe.andThen .clearcoat
                                |> Maybe.andThen .texture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , clearcoatRoughnessTexture =
                            pbr.extensions
                                |> Maybe.andThen .clearcoat
                                |> Maybe.andThen .roughnessTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , clearcoatNormalTexture =
                            pbr.extensions
                                |> Maybe.andThen .clearcoat
                                |> Maybe.andThen .normalTexture
                                |> Maybe.map Gltf.Texture.toIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        , iridescenceTexturePacked =
                            iridescenceTexturePackedIndex
                                |> Maybe.andThen (Gltf.textureWithIndex gltfQueryResult)
                                |> Maybe.withDefault fallbackTexture
                        }
                        (Gltf.Material.Material pbr)

                Err () ->
                    Page.Example.ErrorMaterial.renderer errorMaterialConfig
