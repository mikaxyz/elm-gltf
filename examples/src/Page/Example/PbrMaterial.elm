module Page.Example.PbrMaterial exposing (Config, renderer)

import Gltf.Material
import Gltf.Material.Extensions exposing (TextureTransformExtension)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight
import XYZMika.XYZ.Scene.Light.PointLight as PointLight
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)
import XYZMika.XYZ.Scene.Uniforms as Scene


type alias Config =
    { environmentTexture : WebGL.Texture.Texture
    , specularEnvironmentTexture : WebGL.Texture.Texture
    , brdfLUTTexture : WebGL.Texture.Texture
    }


type alias Uniforms =
    { u_MVPMatrix : Mat4
    , u_ModelMatrix : Mat4
    , u_NormalMatrix : Mat4
    , u_Camera : Vec3
    , u_LightDirection : Vec3
    , u_LightColor : Vec3
    , u_AlphaCutoff : Float

    --
    , u_MetallicRoughnessCoord : Int
    , u_hasMetallicRoughnessSampler : Int
    , u_MetallicRoughnessSampler : Texture
    , u_MetallicRoughnessValues : Vec2
    , u_MetallicRoughnessTransformScale : Vec2
    , u_MetallicRoughnessTransformOffset : Vec2
    , u_MetallicRoughnessTransformRotation : Float

    --
    , u_BaseColorCoord : Int
    , u_hasBaseColorSampler : Int
    , u_BaseColorSampler : Texture
    , u_BaseColorFactor : Vec4
    , u_BaseColorTransformScale : Vec2
    , u_BaseColorTransformOffset : Vec2
    , u_BaseColorTransformRotation : Float

    --
    , u_NormalCoord : Int
    , u_hasNormalSampler : Int
    , u_NormalSampler : Texture
    , u_NormalScale : Float
    , u_NormalTransformScale : Vec2
    , u_NormalTransformOffset : Vec2
    , u_NormalTransformRotation : Float

    --
    , u_OcclusionCoord : Int
    , u_hasOcclusionSampler : Int
    , u_OcclusionSampler : Texture
    , u_OcclusionStrength : Float
    , u_OcclusionTransformScale : Vec2
    , u_OcclusionTransformOffset : Vec2
    , u_OcclusionTransformRotation : Float

    --
    , u_EmissiveCoord : Int
    , u_hasEmissiveSampler : Int
    , u_EmissiveSampler : Texture
    , u_EmissiveFactor : Vec3
    , u_EmissiveTransformScale : Vec2
    , u_EmissiveTransformOffset : Vec2
    , u_EmissiveTransformRotation : Float

    --
    , u_brdfLUT : Texture
    , u_DiffuseEnvSampler : Texture
    , u_SpecularEnvSampler : Texture

    --
    , directionalLight : Vec4
    , pointLight1 : Vec4
    , pointLight1Color : Vec3
    , pointLight2 : Vec4
    , pointLight2Color : Vec3
    , pointLight3 : Vec4
    , pointLight3Color : Vec3
    , pointLight4 : Vec4
    , pointLight4Color : Vec3
    , pointLight5 : Vec4
    , pointLight5Color : Vec3

    --
    , hasBoneTransforms : Int
    , joint0 : Mat4
    , joint1 : Mat4
    , joint2 : Mat4
    , joint3 : Mat4
    , joint4 : Mat4
    , joint5 : Mat4
    , joint6 : Mat4
    , joint7 : Mat4
    , joint8 : Mat4
    , joint9 : Mat4
    , joint10 : Mat4
    , joint11 : Mat4
    , joint12 : Mat4
    , joint13 : Mat4
    , joint14 : Mat4
    , joint15 : Mat4
    , joint16 : Mat4
    , joint17 : Mat4
    , joint18 : Mat4
    , joint19 : Mat4
    , joint20 : Mat4
    , joint21 : Mat4
    , joint22 : Mat4
    , joint23 : Mat4
    , joint24 : Mat4
    , joint25 : Mat4
    , joint26 : Mat4
    , joint27 : Mat4
    , joint28 : Mat4
    , joint29 : Mat4
    , joint30 : Mat4
    , joint31 : Mat4
    , joint32 : Mat4
    , joint33 : Mat4
    , joint34 : Mat4
    , joint35 : Mat4
    , joint36 : Mat4
    , joint37 : Mat4
    , joint38 : Mat4
    , joint39 : Mat4
    , joint40 : Mat4
    , joint41 : Mat4
    , joint42 : Mat4
    , joint43 : Mat4
    , joint44 : Mat4
    , joint45 : Mat4
    , joint46 : Mat4
    , joint47 : Mat4
    , joint48 : Mat4
    , joint49 : Mat4
    , joint50 : Mat4
    , joint51 : Mat4
    , joint52 : Mat4
    , joint53 : Mat4
    , joint54 : Mat4
    , joint55 : Mat4
    , joint56 : Mat4
    , joint57 : Mat4
    , joint58 : Mat4
    , joint59 : Mat4
    , inverseBindMatrix0 : Mat4
    , inverseBindMatrix1 : Mat4
    , inverseBindMatrix2 : Mat4
    , inverseBindMatrix3 : Mat4
    , inverseBindMatrix4 : Mat4
    , inverseBindMatrix5 : Mat4
    , inverseBindMatrix6 : Mat4
    , inverseBindMatrix7 : Mat4
    , inverseBindMatrix8 : Mat4
    , inverseBindMatrix9 : Mat4
    , inverseBindMatrix10 : Mat4
    , inverseBindMatrix11 : Mat4
    , inverseBindMatrix12 : Mat4
    , inverseBindMatrix13 : Mat4
    , inverseBindMatrix14 : Mat4
    , inverseBindMatrix15 : Mat4
    , inverseBindMatrix16 : Mat4
    , inverseBindMatrix17 : Mat4
    , inverseBindMatrix18 : Mat4
    , inverseBindMatrix19 : Mat4
    , inverseBindMatrix20 : Mat4
    , inverseBindMatrix21 : Mat4
    , inverseBindMatrix22 : Mat4
    , inverseBindMatrix23 : Mat4
    , inverseBindMatrix24 : Mat4
    , inverseBindMatrix25 : Mat4
    , inverseBindMatrix26 : Mat4
    , inverseBindMatrix27 : Mat4
    , inverseBindMatrix28 : Mat4
    , inverseBindMatrix29 : Mat4
    , inverseBindMatrix30 : Mat4
    , inverseBindMatrix31 : Mat4
    , inverseBindMatrix32 : Mat4
    , inverseBindMatrix33 : Mat4
    , inverseBindMatrix34 : Mat4
    , inverseBindMatrix35 : Mat4
    , inverseBindMatrix36 : Mat4
    , inverseBindMatrix37 : Mat4
    , inverseBindMatrix38 : Mat4
    , inverseBindMatrix39 : Mat4
    , inverseBindMatrix40 : Mat4
    , inverseBindMatrix41 : Mat4
    , inverseBindMatrix42 : Mat4
    , inverseBindMatrix43 : Mat4
    , inverseBindMatrix44 : Mat4
    , inverseBindMatrix45 : Mat4
    , inverseBindMatrix46 : Mat4
    , inverseBindMatrix47 : Mat4
    , inverseBindMatrix48 : Mat4
    , inverseBindMatrix49 : Mat4
    , inverseBindMatrix50 : Mat4
    , inverseBindMatrix51 : Mat4
    , inverseBindMatrix52 : Mat4
    , inverseBindMatrix53 : Mat4
    , inverseBindMatrix54 : Mat4
    , inverseBindMatrix55 : Mat4
    , inverseBindMatrix56 : Mat4
    , inverseBindMatrix57 : Mat4
    , inverseBindMatrix58 : Mat4
    , inverseBindMatrix59 : Mat4
    }


type alias Varyings =
    { v_Position : Vec3
    , v_Color : Vec3
    , v_UV_0 : Vec2
    , v_UV_1 : Vec2
    , v_Normal : Vec3
    }


renderer :
    Config
    ->
        { pbrMetallicRoughness :
            { baseColorTexture : Texture
            , metallicRoughnessTexture : Texture
            }
        , normalTexture : Texture
        , occlusionTexture : Texture
        , emissiveTexture : Texture
        }
    -> Gltf.Material.Material
    -> Material.Options
    -> Scene.Uniforms u
    -> Object objectId materialId
    -> Entity
renderer config textures (Gltf.Material.Material pbr) options uniforms object =
    let
        boneTransforms : BoneTransforms
        boneTransforms =
            Object.boneTransforms object
                |> Maybe.withDefault Object.boneTransformsIdentity

        hasBoneTransforms : Int
        hasBoneTransforms =
            if Object.boneTransforms object /= Nothing then
                1

            else
                0

        pointLight : Int -> { light : Vec4, color : Vec3 }
        pointLight i =
            options
                |> Material.pointLightByIndex (i - 1)
                |> Maybe.map
                    (\light ->
                        { light = PointLight.toVec4 light
                        , color = PointLight.color light
                        }
                    )
                |> Maybe.withDefault
                    { light = vec4 0 0 0 0
                    , color = vec3 0 0 0
                    }

        directionalLight : Vec4
        directionalLight =
            options
                |> Material.directionalLights
                |> List.head
                |> Maybe.map DirectionalLight.toVec4
                |> Maybe.withDefault (vec4 0 0 0 0)

        settingsWithAlpha : List WebGL.Settings.Setting -> List WebGL.Settings.Setting
        settingsWithAlpha x =
            case pbr.alphaMode of
                Gltf.Material.Opaque ->
                    x

                Gltf.Material.Mask _ ->
                    x

                Gltf.Material.Blend ->
                    WebGL.Settings.sampleCoverage 1.0 True
                        :: WebGL.Settings.sampleAlphaToCoverage
                        :: x

        alphaCutoff : Float
        alphaCutoff =
            case pbr.alphaMode of
                Gltf.Material.Opaque ->
                    -2.0

                Gltf.Material.Mask cutoff ->
                    cutoff

                Gltf.Material.Blend ->
                    -1.0

        settings : List WebGL.Settings.Setting
        settings =
            [ ( True, WebGL.Settings.DepthTest.default )
            , ( not pbr.doubleSided, WebGL.Settings.cullFace WebGL.Settings.back )
            ]
                |> List.filter Tuple.first
                |> List.map Tuple.second
                |> settingsWithAlpha

        flagFromMaybe : Maybe a -> number
        flagFromMaybe x =
            if x /= Nothing then
                1

            else
                0

        textureTransform : Maybe Gltf.Material.Texture -> Maybe TextureTransformExtension
        textureTransform texture =
            texture
                |> Maybe.andThen (\(Gltf.Material.Texture x) -> x.extensions)
                |> Maybe.andThen (\extensions -> extensions.textureTransform)

        textureScale : Maybe Gltf.Material.Texture -> Vec2
        textureScale texture =
            textureTransform texture
                |> Maybe.map .scale
                |> Maybe.withDefault (vec2 1 1)

        textureOffset : Maybe Gltf.Material.Texture -> Vec2
        textureOffset texture =
            textureTransform texture
                |> Maybe.map .offset
                |> Maybe.withDefault (vec2 0 0)

        textureRotation : Maybe Gltf.Material.Texture -> Float
        textureRotation texture =
            textureTransform texture
                |> Maybe.map .rotation
                |> Maybe.withDefault 0

        texCoord : Maybe Gltf.Material.Texture -> Int
        texCoord texture =
            texture
                |> Maybe.map (\(Gltf.Material.Texture x) -> x.texCoord)
                |> Maybe.withDefault 0
    in
    material
        { u_MVPMatrix = Mat4.mul (Mat4.mul uniforms.scenePerspective uniforms.sceneCamera) uniforms.sceneMatrix
        , u_ModelMatrix = uniforms.sceneMatrix
        , u_NormalMatrix = uniforms.sceneRotationMatrix
        , u_Camera = Camera.position (Material.camera options)

        --
        , u_LightDirection = vec3 1 1 1
        , u_LightColor = vec3 0.6 0.59 0.56
        , u_AlphaCutoff = alphaCutoff

        --
        , u_MetallicRoughnessCoord = texCoord pbr.pbrMetallicRoughness.metallicRoughnessTexture
        , u_MetallicRoughnessValues = vec2 pbr.pbrMetallicRoughness.metallicFactor pbr.pbrMetallicRoughness.roughnessFactor
        , u_hasMetallicRoughnessSampler = pbr.pbrMetallicRoughness.metallicRoughnessTexture |> flagFromMaybe
        , u_MetallicRoughnessSampler = textures.pbrMetallicRoughness.metallicRoughnessTexture
        , u_MetallicRoughnessTransformScale = textureScale pbr.pbrMetallicRoughness.metallicRoughnessTexture
        , u_MetallicRoughnessTransformOffset = textureOffset pbr.pbrMetallicRoughness.metallicRoughnessTexture
        , u_MetallicRoughnessTransformRotation = textureRotation pbr.pbrMetallicRoughness.metallicRoughnessTexture

        --
        , u_BaseColorCoord = texCoord pbr.pbrMetallicRoughness.baseColorTexture
        , u_BaseColorFactor = pbr.pbrMetallicRoughness.baseColorFactor
        , u_hasBaseColorSampler = pbr.pbrMetallicRoughness.baseColorTexture |> flagFromMaybe
        , u_BaseColorSampler = textures.pbrMetallicRoughness.baseColorTexture
        , u_BaseColorTransformScale = textureScale pbr.pbrMetallicRoughness.baseColorTexture
        , u_BaseColorTransformOffset = textureOffset pbr.pbrMetallicRoughness.baseColorTexture
        , u_BaseColorTransformRotation = textureRotation pbr.pbrMetallicRoughness.baseColorTexture

        --
        , u_NormalCoord = texCoord pbr.normalTexture
        , u_hasNormalSampler = pbr.normalTexture |> flagFromMaybe
        , u_NormalSampler = textures.normalTexture
        , u_NormalScale = pbr.normalTextureScale
        , u_NormalTransformScale = textureScale pbr.normalTexture
        , u_NormalTransformOffset = textureOffset pbr.normalTexture
        , u_NormalTransformRotation = textureRotation pbr.normalTexture

        --
        , u_OcclusionCoord = texCoord pbr.occlusionTexture
        , u_hasOcclusionSampler = pbr.occlusionTexture |> flagFromMaybe
        , u_OcclusionSampler = textures.occlusionTexture
        , u_OcclusionStrength = pbr.occlusionTextureStrength
        , u_OcclusionTransformScale = textureScale pbr.occlusionTexture
        , u_OcclusionTransformOffset = textureOffset pbr.occlusionTexture
        , u_OcclusionTransformRotation = textureRotation pbr.occlusionTexture

        --
        , u_EmissiveCoord = texCoord pbr.emissiveTexture
        , u_hasEmissiveSampler = pbr.emissiveTexture |> flagFromMaybe
        , u_EmissiveSampler = textures.emissiveTexture
        , u_EmissiveFactor = pbr.emissiveFactor
        , u_EmissiveTransformScale = textureScale pbr.emissiveTexture
        , u_EmissiveTransformOffset = textureOffset pbr.emissiveTexture
        , u_EmissiveTransformRotation = textureRotation pbr.emissiveTexture

        --
        , u_brdfLUT = config.brdfLUTTexture
        , u_DiffuseEnvSampler = config.environmentTexture
        , u_SpecularEnvSampler = config.specularEnvironmentTexture

        --
        , directionalLight = directionalLight
        , pointLight1 = pointLight 1 |> .light
        , pointLight1Color = pointLight 1 |> .color
        , pointLight2 = pointLight 2 |> .light
        , pointLight2Color = pointLight 2 |> .color
        , pointLight3 = pointLight 3 |> .light
        , pointLight3Color = pointLight 3 |> .color
        , pointLight4 = pointLight 4 |> .light
        , pointLight4Color = pointLight 4 |> .color
        , pointLight5 = pointLight 5 |> .light
        , pointLight5Color = pointLight 5 |> .color

        --
        , hasBoneTransforms = hasBoneTransforms
        , joint0 = boneTransforms.joint0
        , joint1 = boneTransforms.joint1
        , joint2 = boneTransforms.joint2
        , joint3 = boneTransforms.joint3
        , joint4 = boneTransforms.joint4
        , joint5 = boneTransforms.joint5
        , joint6 = boneTransforms.joint6
        , joint7 = boneTransforms.joint7
        , joint8 = boneTransforms.joint8
        , joint9 = boneTransforms.joint9
        , joint10 = boneTransforms.joint10
        , joint11 = boneTransforms.joint11
        , joint12 = boneTransforms.joint12
        , joint13 = boneTransforms.joint13
        , joint14 = boneTransforms.joint14
        , joint15 = boneTransforms.joint15
        , joint16 = boneTransforms.joint16
        , joint17 = boneTransforms.joint17
        , joint18 = boneTransforms.joint18
        , joint19 = boneTransforms.joint19
        , joint20 = boneTransforms.joint20
        , joint21 = boneTransforms.joint21
        , joint22 = boneTransforms.joint22
        , joint23 = boneTransforms.joint23
        , joint24 = boneTransforms.joint24
        , joint25 = boneTransforms.joint25
        , joint26 = boneTransforms.joint26
        , joint27 = boneTransforms.joint27
        , joint28 = boneTransforms.joint28
        , joint29 = boneTransforms.joint29
        , joint30 = boneTransforms.joint30
        , joint31 = boneTransforms.joint31
        , joint32 = boneTransforms.joint32
        , joint33 = boneTransforms.joint33
        , joint34 = boneTransforms.joint34
        , joint35 = boneTransforms.joint35
        , joint36 = boneTransforms.joint36
        , joint37 = boneTransforms.joint37
        , joint38 = boneTransforms.joint38
        , joint39 = boneTransforms.joint39
        , joint40 = boneTransforms.joint40
        , joint41 = boneTransforms.joint41
        , joint42 = boneTransforms.joint42
        , joint43 = boneTransforms.joint43
        , joint44 = boneTransforms.joint44
        , joint45 = boneTransforms.joint45
        , joint46 = boneTransforms.joint46
        , joint47 = boneTransforms.joint47
        , joint48 = boneTransforms.joint48
        , joint49 = boneTransforms.joint49
        , joint50 = boneTransforms.joint50
        , joint51 = boneTransforms.joint51
        , joint52 = boneTransforms.joint52
        , joint53 = boneTransforms.joint53
        , joint54 = boneTransforms.joint54
        , joint55 = boneTransforms.joint55
        , joint56 = boneTransforms.joint56
        , joint57 = boneTransforms.joint57
        , joint58 = boneTransforms.joint58
        , joint59 = boneTransforms.joint59

        --
        , inverseBindMatrix0 = boneTransforms.inverseBindMatrix0
        , inverseBindMatrix1 = boneTransforms.inverseBindMatrix1
        , inverseBindMatrix2 = boneTransforms.inverseBindMatrix2
        , inverseBindMatrix3 = boneTransforms.inverseBindMatrix3
        , inverseBindMatrix4 = boneTransforms.inverseBindMatrix4
        , inverseBindMatrix5 = boneTransforms.inverseBindMatrix5
        , inverseBindMatrix6 = boneTransforms.inverseBindMatrix6
        , inverseBindMatrix7 = boneTransforms.inverseBindMatrix7
        , inverseBindMatrix8 = boneTransforms.inverseBindMatrix8
        , inverseBindMatrix9 = boneTransforms.inverseBindMatrix9
        , inverseBindMatrix10 = boneTransforms.inverseBindMatrix10
        , inverseBindMatrix11 = boneTransforms.inverseBindMatrix11
        , inverseBindMatrix12 = boneTransforms.inverseBindMatrix12
        , inverseBindMatrix13 = boneTransforms.inverseBindMatrix13
        , inverseBindMatrix14 = boneTransforms.inverseBindMatrix14
        , inverseBindMatrix15 = boneTransforms.inverseBindMatrix15
        , inverseBindMatrix16 = boneTransforms.inverseBindMatrix16
        , inverseBindMatrix17 = boneTransforms.inverseBindMatrix17
        , inverseBindMatrix18 = boneTransforms.inverseBindMatrix18
        , inverseBindMatrix19 = boneTransforms.inverseBindMatrix19
        , inverseBindMatrix20 = boneTransforms.inverseBindMatrix20
        , inverseBindMatrix21 = boneTransforms.inverseBindMatrix21
        , inverseBindMatrix22 = boneTransforms.inverseBindMatrix22
        , inverseBindMatrix23 = boneTransforms.inverseBindMatrix23
        , inverseBindMatrix24 = boneTransforms.inverseBindMatrix24
        , inverseBindMatrix25 = boneTransforms.inverseBindMatrix25
        , inverseBindMatrix26 = boneTransforms.inverseBindMatrix26
        , inverseBindMatrix27 = boneTransforms.inverseBindMatrix27
        , inverseBindMatrix28 = boneTransforms.inverseBindMatrix28
        , inverseBindMatrix29 = boneTransforms.inverseBindMatrix29
        , inverseBindMatrix30 = boneTransforms.inverseBindMatrix30
        , inverseBindMatrix31 = boneTransforms.inverseBindMatrix31
        , inverseBindMatrix32 = boneTransforms.inverseBindMatrix32
        , inverseBindMatrix33 = boneTransforms.inverseBindMatrix33
        , inverseBindMatrix34 = boneTransforms.inverseBindMatrix34
        , inverseBindMatrix35 = boneTransforms.inverseBindMatrix35
        , inverseBindMatrix36 = boneTransforms.inverseBindMatrix36
        , inverseBindMatrix37 = boneTransforms.inverseBindMatrix37
        , inverseBindMatrix38 = boneTransforms.inverseBindMatrix38
        , inverseBindMatrix39 = boneTransforms.inverseBindMatrix39
        , inverseBindMatrix40 = boneTransforms.inverseBindMatrix40
        , inverseBindMatrix41 = boneTransforms.inverseBindMatrix41
        , inverseBindMatrix42 = boneTransforms.inverseBindMatrix42
        , inverseBindMatrix43 = boneTransforms.inverseBindMatrix43
        , inverseBindMatrix44 = boneTransforms.inverseBindMatrix44
        , inverseBindMatrix45 = boneTransforms.inverseBindMatrix45
        , inverseBindMatrix46 = boneTransforms.inverseBindMatrix46
        , inverseBindMatrix47 = boneTransforms.inverseBindMatrix47
        , inverseBindMatrix48 = boneTransforms.inverseBindMatrix48
        , inverseBindMatrix49 = boneTransforms.inverseBindMatrix49
        , inverseBindMatrix50 = boneTransforms.inverseBindMatrix50
        , inverseBindMatrix51 = boneTransforms.inverseBindMatrix51
        , inverseBindMatrix52 = boneTransforms.inverseBindMatrix52
        , inverseBindMatrix53 = boneTransforms.inverseBindMatrix53
        , inverseBindMatrix54 = boneTransforms.inverseBindMatrix54
        , inverseBindMatrix55 = boneTransforms.inverseBindMatrix55
        , inverseBindMatrix56 = boneTransforms.inverseBindMatrix56
        , inverseBindMatrix57 = boneTransforms.inverseBindMatrix57
        , inverseBindMatrix58 = boneTransforms.inverseBindMatrix58
        , inverseBindMatrix59 = boneTransforms.inverseBindMatrix59
        }
        |> Material.toEntityWithSettings settings object


material : Uniforms -> Material Uniforms Varyings
material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        // Based on https://github.com/bghgary/glTF-WebGL-PBR/tree/master/shaders
        precision highp float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 tangent;
        attribute vec3 color;
        attribute vec2 uv;
        attribute vec2 uv1;
        attribute vec4 joints;
        attribute vec4 weights;

        uniform mat4 u_MVPMatrix;
        uniform mat4 u_ModelMatrix;
        uniform mat4 u_NormalMatrix;

        varying vec3 v_Position;
        varying vec3 v_Color;
        varying vec2 v_UV_0;
        varying vec2 v_UV_1;
        varying vec3 v_Normal;

        uniform int hasBoneTransforms;
        uniform mat4 joint0;
        uniform mat4 joint1;
        uniform mat4 joint2;
        uniform mat4 joint3;
        uniform mat4 joint4;
        uniform mat4 joint5;
        uniform mat4 joint6;
        uniform mat4 joint7;
        uniform mat4 joint8;
        uniform mat4 joint9;
        uniform mat4 joint10;
        uniform mat4 joint11;
        uniform mat4 joint12;
        uniform mat4 joint13;
        uniform mat4 joint14;
        uniform mat4 joint15;
        uniform mat4 joint16;
        uniform mat4 joint17;
        uniform mat4 joint18;
        uniform mat4 joint19;
        uniform mat4 joint20;
        uniform mat4 joint21;
        uniform mat4 joint22;
        uniform mat4 joint23;
        uniform mat4 joint24;
        uniform mat4 joint25;
        uniform mat4 joint26;
        uniform mat4 joint27;
        uniform mat4 joint28;
        uniform mat4 joint29;
        uniform mat4 joint30;
        uniform mat4 joint31;
        uniform mat4 joint32;
        uniform mat4 joint33;
        uniform mat4 joint34;
        uniform mat4 joint35;
        uniform mat4 joint36;
        uniform mat4 joint37;
        uniform mat4 joint38;
        uniform mat4 joint39;
        uniform mat4 joint40;
        uniform mat4 joint41;
        uniform mat4 joint42;
        uniform mat4 joint43;
        uniform mat4 joint44;
        uniform mat4 joint45;
        uniform mat4 joint46;
        uniform mat4 joint47;
        uniform mat4 joint48;
        uniform mat4 joint49;
        uniform mat4 joint50;
        uniform mat4 joint51;
        uniform mat4 joint52;
        uniform mat4 joint53;
        uniform mat4 joint54;
        uniform mat4 joint55;
        uniform mat4 joint56;
        uniform mat4 joint57;
        uniform mat4 joint58;
        uniform mat4 joint59;

        uniform mat4 inverseBindMatrix0;
        uniform mat4 inverseBindMatrix1;
        uniform mat4 inverseBindMatrix2;
        uniform mat4 inverseBindMatrix3;
        uniform mat4 inverseBindMatrix4;
        uniform mat4 inverseBindMatrix5;
        uniform mat4 inverseBindMatrix6;
        uniform mat4 inverseBindMatrix7;
        uniform mat4 inverseBindMatrix8;
        uniform mat4 inverseBindMatrix9;
        uniform mat4 inverseBindMatrix10;
        uniform mat4 inverseBindMatrix11;
        uniform mat4 inverseBindMatrix12;
        uniform mat4 inverseBindMatrix13;
        uniform mat4 inverseBindMatrix14;
        uniform mat4 inverseBindMatrix15;
        uniform mat4 inverseBindMatrix16;
        uniform mat4 inverseBindMatrix17;
        uniform mat4 inverseBindMatrix18;
        uniform mat4 inverseBindMatrix19;
        uniform mat4 inverseBindMatrix20;
        uniform mat4 inverseBindMatrix21;
        uniform mat4 inverseBindMatrix22;
        uniform mat4 inverseBindMatrix23;
        uniform mat4 inverseBindMatrix24;
        uniform mat4 inverseBindMatrix25;
        uniform mat4 inverseBindMatrix26;
        uniform mat4 inverseBindMatrix27;
        uniform mat4 inverseBindMatrix28;
        uniform mat4 inverseBindMatrix29;
        uniform mat4 inverseBindMatrix30;
        uniform mat4 inverseBindMatrix31;
        uniform mat4 inverseBindMatrix32;
        uniform mat4 inverseBindMatrix33;
        uniform mat4 inverseBindMatrix34;
        uniform mat4 inverseBindMatrix35;
        uniform mat4 inverseBindMatrix36;
        uniform mat4 inverseBindMatrix37;
        uniform mat4 inverseBindMatrix38;
        uniform mat4 inverseBindMatrix39;
        uniform mat4 inverseBindMatrix40;
        uniform mat4 inverseBindMatrix41;
        uniform mat4 inverseBindMatrix42;
        uniform mat4 inverseBindMatrix43;
        uniform mat4 inverseBindMatrix44;
        uniform mat4 inverseBindMatrix45;
        uniform mat4 inverseBindMatrix46;
        uniform mat4 inverseBindMatrix47;
        uniform mat4 inverseBindMatrix48;
        uniform mat4 inverseBindMatrix49;
        uniform mat4 inverseBindMatrix50;
        uniform mat4 inverseBindMatrix51;
        uniform mat4 inverseBindMatrix52;
        uniform mat4 inverseBindMatrix53;
        uniform mat4 inverseBindMatrix54;
        uniform mat4 inverseBindMatrix55;
        uniform mat4 inverseBindMatrix56;
        uniform mat4 inverseBindMatrix57;
        uniform mat4 inverseBindMatrix58;
        uniform mat4 inverseBindMatrix59;

        mat4 jointMat(int i, float weights) {
            mat4 m = mat4(1);
            if (i == 0) {
                m = joint0 * inverseBindMatrix0;
            }
            if (i == 1) {
                m = joint1 * inverseBindMatrix1;
            }
            if (i == 2) {
                m = joint2 * inverseBindMatrix2;
            }
            if (i == 3) {
                m = joint3 * inverseBindMatrix3;
            }
            if (i == 4) {
                m = joint4 * inverseBindMatrix4;
            }
            if (i == 5) {
                m = joint5 * inverseBindMatrix5;
            }
            if (i == 6) {
                m = joint6 * inverseBindMatrix6;
            }
            if (i == 7) {
                m = joint7 * inverseBindMatrix7;
            }
            if (i == 8) {
                m = joint8 * inverseBindMatrix8;
            }
            if (i == 9) {
                m = joint9 * inverseBindMatrix9;
            }
            if (i == 10) {
                m = joint10 * inverseBindMatrix10;
            }
            if (i == 11) {
                m = joint11 * inverseBindMatrix11;
            }
            if (i == 12) {
                m = joint12 * inverseBindMatrix12;
            }
            if (i == 13) {
                m = joint13 * inverseBindMatrix13;
            }
            if (i == 14) {
                m = joint14 * inverseBindMatrix14;
            }
            if (i == 15) {
                m = joint15 * inverseBindMatrix15;
            }
            if (i == 16) {
                m = joint16 * inverseBindMatrix16;
            }
            if (i == 17) {
                m = joint17 * inverseBindMatrix17;
            }
            if (i == 18) {
                m = joint18 * inverseBindMatrix18;
            }
            if (i == 19) {
                m = joint19 * inverseBindMatrix19;
            }
            if (i == 20) {
                m = joint20 * inverseBindMatrix20;
            }
            if (i == 21) {
                m = joint21 * inverseBindMatrix21;
            }
            if (i == 22) {
                m = joint22 * inverseBindMatrix22;
            }
            if (i == 23) {
                m = joint23 * inverseBindMatrix23;
            }
            if (i == 24) {
                m = joint24 * inverseBindMatrix24;
            }
            if (i == 25) {
                m = joint25 * inverseBindMatrix25;
            }
            if (i == 26) {
                m = joint26 * inverseBindMatrix26;
            }
            if (i == 27) {
                m = joint27 * inverseBindMatrix27;
            }
            if (i == 28) {
                m = joint28 * inverseBindMatrix28;
            }
            if (i == 29) {
                m = joint29 * inverseBindMatrix29;
            }

            if (i == 30) {
                m = joint30 * inverseBindMatrix30;
            }
            if (i == 31) {
                m = joint31 * inverseBindMatrix31;
            }
            if (i == 32) {
                m = joint32 * inverseBindMatrix32;
            }
            if (i == 33) {
                m = joint33 * inverseBindMatrix33;
            }
            if (i == 34) {
                m = joint34 * inverseBindMatrix34;
            }
            if (i == 35) {
                m = joint35 * inverseBindMatrix35;
            }
            if (i == 36) {
                m = joint36 * inverseBindMatrix36;
            }
            if (i == 37) {
                m = joint37 * inverseBindMatrix37;
            }
            if (i == 38) {
                m = joint38 * inverseBindMatrix38;
            }
            if (i == 39) {
                m = joint39 * inverseBindMatrix39;
            }
            if (i == 40) {
                m = joint40 * inverseBindMatrix40;
            }
            if (i == 41) {
                m = joint41 * inverseBindMatrix41;
            }
            if (i == 42) {
                m = joint42 * inverseBindMatrix42;
            }
            if (i == 43) {
                m = joint43 * inverseBindMatrix43;
            }
            if (i == 44) {
                m = joint44 * inverseBindMatrix44;
            }
            if (i == 45) {
                m = joint45 * inverseBindMatrix45;
            }
            if (i == 46) {
                m = joint46 * inverseBindMatrix46;
            }
            if (i == 47) {
                m = joint47 * inverseBindMatrix47;
            }
            if (i == 48) {
                m = joint48 * inverseBindMatrix48;
            }
            if (i == 49) {
                m = joint49 * inverseBindMatrix49;
            }
            if (i == 50) {
                m = joint50 * inverseBindMatrix50;
            }
            if (i == 51) {
                m = joint51 * inverseBindMatrix51;
            }
            if (i == 52) {
                m = joint52 * inverseBindMatrix52;
            }
            if (i == 53) {
                m = joint53 * inverseBindMatrix53;
            }
            if (i == 54) {
                m = joint54 * inverseBindMatrix54;
            }
            if (i == 55) {
                m = joint55 * inverseBindMatrix55;
            }
            if (i == 56) {
                m = joint56 * inverseBindMatrix56;
            }
            if (i == 57) {
                m = joint57 * inverseBindMatrix57;
            }
            if (i == 58) {
                m = joint58 * inverseBindMatrix58;
            }
            if (i == 59) {
                m = joint59 * inverseBindMatrix59;
            }
            return m * weights;
        }

        void main () {
            mat4 skinDeform = mat4(1);
            mat4 modelMatrix = u_ModelMatrix;
            if (hasBoneTransforms == 1) {
                skinDeform =
                    jointMat(int(joints.x), weights.x) +
                    jointMat(int(joints.y), weights.y) +
                    jointMat(int(joints.z), weights.z) +
                    jointMat(int(joints.w), weights.w);

                modelMatrix = modelMatrix * skinDeform;
            }

            vec4 pos = modelMatrix * vec4(position, 1.0);
            v_Position = vec3(pos.xyz) / pos.w;
            v_Normal = normalize(vec3(modelMatrix * vec4(normal.xyz, 0.0)));
            v_UV_0 = uv;
            v_UV_1 = uv1;
            v_Color = color;

            gl_Position = u_MVPMatrix * skinDeform * vec4(position, 1.0); // needs w for proper perspective correction
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        // Based on https://github.com/bghgary/glTF-WebGL-PBR/tree/master/shaders
        precision highp float;

        uniform vec3 u_LightDirection;
        uniform vec3 u_LightColor;
        uniform float u_AlphaCutoff;

        uniform sampler2D u_brdfLUT;
        uniform samplerCube u_DiffuseEnvSampler;
        uniform samplerCube u_SpecularEnvSampler;

        uniform int u_MetallicRoughnessCoord;
        uniform vec2 u_MetallicRoughnessValues;
        uniform int u_hasMetallicRoughnessSampler;
        uniform sampler2D u_MetallicRoughnessSampler;
        uniform vec2 u_MetallicRoughnessTransformScale;
        uniform vec2 u_MetallicRoughnessTransformOffset;
        uniform float u_MetallicRoughnessTransformRotation;

        uniform int u_BaseColorCoord;
        uniform vec4 u_BaseColorFactor;
        uniform int u_hasBaseColorSampler;
        uniform sampler2D u_BaseColorSampler;
        uniform vec2 u_BaseColorTransformScale;
        uniform vec2 u_BaseColorTransformOffset;
        uniform float u_BaseColorTransformRotation;

        uniform int u_NormalCoord;
        uniform int u_hasNormalSampler;
        uniform sampler2D u_NormalSampler;
        uniform float u_NormalScale;
        uniform vec2 u_NormalTransformScale;
        uniform vec2 u_NormalTransformOffset;
        uniform float u_NormalTransformRotation;

        uniform int u_OcclusionCoord;
        uniform int u_hasOcclusionSampler;
        uniform sampler2D u_OcclusionSampler;
        uniform float u_OcclusionStrength;
        uniform vec2 u_OcclusionTransformScale;
        uniform vec2 u_OcclusionTransformOffset;
        uniform float u_OcclusionTransformRotation;

        uniform int u_EmissiveCoord;
        uniform int u_hasEmissiveSampler;
        uniform sampler2D u_EmissiveSampler;
        uniform vec3 u_EmissiveFactor;
        uniform vec2 u_EmissiveTransformScale;
        uniform vec2 u_EmissiveTransformOffset;
        uniform float u_EmissiveTransformRotation;

        uniform vec3 u_Camera;

        varying vec3 v_Position;
        varying vec3 v_Color;
        varying vec2 v_UV_0;
        varying vec2 v_UV_1;
        varying vec3 v_Normal;

        // Encapsulate the various inputs used by the various functions in the shading equation
        // We store values in this struct to simplify the integration of alternative implementations
        // of the shading terms, outlined in the Readme.MD Appendix.
        struct PBRInfo
        {
            float NdotL;                  // cos angle between normal and light direction
            float NdotV;                  // cos angle between normal and view direction
            float NdotH;                  // cos angle between normal and half vector
            float LdotH;                  // cos angle between light direction and half vector
            float VdotH;                  // cos angle between view direction and half vector
            float perceptualRoughness;    // roughness value, as authored by the model creator (input to shader)
            float metalness;              // metallic value at the surface
            vec3 reflectance0;            // full reflectance color (normal incidence angle)
            vec3 reflectance90;           // reflectance color at grazing angle
            float alphaRoughness;         // roughness mapped to a more linear change in the roughness (proposed by [2])
            vec3 diffuseColor;            // color contribution from diffuse lighting
            vec3 specularColor;           // color contribution from specular lighting
        };


        const float M_PI = 3.141592653589793;
        const float c_MinRoughness = 0.04;

        vec4 SRGBtoLINEAR(vec4 srgbIn)
        {
            vec3 bLess = step(vec3(0.04045),srgbIn.xyz);
            vec3 linOut = mix( srgbIn.xyz/vec3(12.92), pow((srgbIn.xyz+vec3(0.055))/vec3(1.055),vec3(2.4)), bLess );
            return vec4(linOut,srgbIn.w);
        }

//        vec3 f_pointLight (vec3 normal, vec3 lightPosition) {
//            highp vec3 color = vec3(1.0, 1.0, 1.0);
//            highp vec3 pointLightDirection = normalize(lightPosition - v_fragPos);
//            highp float pointLightDiff = max(dot(normal, pointLightDirection), 0.0);
//            highp float intensity = pow(pointLightDiff, 1.0);
//            highp float distance = distance(lightPosition, v_fragPos);
//
//            return color * intensity;
//        }
//
//        vec3 f_directionalLight (vec3 normal) {
//            highp vec3 color = vec3(1.0, 1.0, 1.0);
//            highp vec3 directionalVector = normalize(directionalLight.xyz);
//            highp float intensity = max(dot(normal, directionalVector), 0.0);
//            return color * intensity;
//        }

        vec2 uvFromCoord(int coord) {
            return coord == 0 ? v_UV_0 : v_UV_1;
        }

        vec2 transformedUv(int coord, vec2 Scale, vec2 Offset, float Rotation)
        {
            vec2 uv = uvFromCoord(coord);
            mat3 translation = mat3(1,0,0, 0,1,0, Offset.x, Offset.y, 1);
            float c = cos(-Rotation);
            float s = sin(-Rotation);
            mat3 rotation = mat3(
                c, s, 0,
               -s, c, 0,
                0, 0, 1
            );
            mat3 scale = mat3(Scale.x,0,0, 0,Scale.y,0, 0,0,1);
            mat3 matrix = translation * rotation * scale;
            return ( matrix * vec3(uv, 1) ).xy;
        }

        // Find the normal for this fragment, pulling either from a predefined normal map
        // or from the interpolated mesh normal and tangent attributes.
        vec3 getNormal()
        {
            vec2 uv = uvFromCoord(u_NormalCoord);
            vec3 pos_dx = dFdx(v_Position);
            vec3 pos_dy = dFdy(v_Position);
            vec3 tex_dx = dFdx(vec3(uv, 0.0));
            vec3 tex_dy = dFdy(vec3(uv, 0.0));
            vec3 t = (tex_dy.t * pos_dx - tex_dx.t * pos_dy) / (tex_dx.s * tex_dy.t - tex_dy.s * tex_dx.t);

            vec3 ng = normalize(v_Normal);
            t = normalize(t - ng * dot(ng, t));
            vec3 b = normalize(cross(ng, t));
            mat3 tbn = mat3(t, b, ng);

            vec3 n;
            if (u_hasNormalSampler == 1) {
                vec2 uvTransformed = transformedUv(
                    u_NormalCoord,
                    u_NormalTransformScale,
                    u_NormalTransformOffset,
                    u_NormalTransformRotation
                );
                n = texture2D(u_NormalSampler, uvTransformed).rgb;
                n = normalize(tbn * ((2.0 * n - 1.0) * vec3(u_NormalScale, u_NormalScale, 1.0)));
            } else {
                // The tbn matrix is linearly interpolated, so we need to re-normalize
                n = normalize(tbn[2].xyz);
            }

            return n;
        }

        // Calculation of the lighting contribution from an optional Image Based Light source.
        // Precomputed Environment Maps are required uniform inputs and are computed as outlined in [1].
        // See our README.md on Environment Maps [3] for additional discussion.
        vec3 getIBLContribution(PBRInfo pbrInputs, vec3 n, vec3 reflection)
        {
            float mipCount = 32.0; // resolution of 512x512
            float lod = (pbrInputs.perceptualRoughness * (mipCount + 1.0));
            // retrieve a scale and bias to F0. See [1], Figure 3
            vec3 brdf = SRGBtoLINEAR(texture2D(u_brdfLUT, vec2(pbrInputs.NdotV, 1.0 - pbrInputs.perceptualRoughness))).rgb;
            vec3 diffuseLight = SRGBtoLINEAR(textureCube(u_DiffuseEnvSampler, n)).rgb;
            vec3 specularLight = SRGBtoLINEAR(textureCubeLodEXT(u_SpecularEnvSampler, reflection, lod)).rgb;
            vec3 diffuse = diffuseLight * pbrInputs.diffuseColor;
            vec3 specular = specularLight * (pbrInputs.specularColor * brdf.x + brdf.y);

            return diffuse + specular;
        }

        // Basic Lambertian diffuse
        // Implementation from Lambert's Photometria https://archive.org/details/lambertsphotome00lambgoog
        // See also [1], Equation 1
        vec3 diffuse(PBRInfo pbrInputs)
        {
            return pbrInputs.diffuseColor / M_PI;
        }

        // The following equation models the Fresnel reflectance term of the spec equation (aka F())
        // Implementation of fresnel from [4], Equation 15
        vec3 specularReflection(PBRInfo pbrInputs)
        {
            return pbrInputs.reflectance0 + (pbrInputs.reflectance90 - pbrInputs.reflectance0) * pow(clamp(1.0 - pbrInputs.VdotH, 0.0, 1.0), 5.0);
        }

        // This calculates the specular geometric attenuation (aka G()),
        // where rougher material will reflect less light back to the viewer.
        // This implementation is based on [1] Equation 4, and we adopt their modifications to
        // alphaRoughness as input as originally proposed in [2].
        float geometricOcclusion(PBRInfo pbrInputs)
        {
            float NdotL = pbrInputs.NdotL;
            float NdotV = pbrInputs.NdotV;
            float r = pbrInputs.alphaRoughness;

            float attenuationL = 2.0 * NdotL / (NdotL + sqrt(r * r + (1.0 - r * r) * (NdotL * NdotL)));
            float attenuationV = 2.0 * NdotV / (NdotV + sqrt(r * r + (1.0 - r * r) * (NdotV * NdotV)));
            return attenuationL * attenuationV;
        }

        // The following equation(s) model the distribution of microfacet normals across the area being drawn (aka D())
        // Implementation from "Average Irregularity Representation of a Roughened Surface for Ray Reflection" by T. S. Trowbridge, and K. P. Reitz
        // Follows the distribution function recommended in the SIGGRAPH 2013 course notes from EPIC Games [1], Equation 3.
        float microfacetDistribution(PBRInfo pbrInputs)
        {
            float roughnessSq = pbrInputs.alphaRoughness * pbrInputs.alphaRoughness;
            float f = (pbrInputs.NdotH * roughnessSq - pbrInputs.NdotH) * pbrInputs.NdotH + 1.0;
            return roughnessSq / (M_PI * f * f);
        }

        void main () {
//            vec3 lighting = vec3(0);
//
//            if (pointLight1.w > 0.0) {
//               lighting += pointLight1.w * f_pointLight(normal, pointLight1.xyz) * pointLight1Color;
//            }
//            if (pointLight2.w > 0.0) {
//               lighting += pointLight2.w * f_pointLight(normal, pointLight2.xyz) * pointLight2Color;
//            }
//            if (pointLight3.w > 0.0) {
//               lighting += pointLight3.w * f_pointLight(normal, pointLight3.xyz) * pointLight3Color;
//            }
//            if (pointLight4.w > 0.0) {
//               lighting += pointLight4.w * f_pointLight(normal, pointLight4.xyz) * pointLight4Color;
//            }
//            if (pointLight5.w > 0.0) {
//               lighting += pointLight5.w * f_pointLight(normal, pointLight5.xyz) * pointLight5Color;
//            }
//
//            if (directionalLight.w > 0.0) {
//                lighting += f_directionalLight(normal);
//            }


            // Metallic and Roughness material properties are packed together
            // In glTF, these factors can be specified by fixed scalar values
            // or from a metallic-roughness map
            float perceptualRoughness = u_MetallicRoughnessValues.y;
            float metallic = u_MetallicRoughnessValues.x;

            if (u_hasMetallicRoughnessSampler == 1) {
                vec2 uvTransformed = transformedUv(
                    u_MetallicRoughnessCoord,
                    u_MetallicRoughnessTransformScale,
                    u_MetallicRoughnessTransformOffset,
                    u_MetallicRoughnessTransformRotation
                );
                vec4 mrSample = texture2D(u_MetallicRoughnessSampler, uvTransformed);
                perceptualRoughness = mrSample.g * perceptualRoughness;
                metallic = mrSample.b * metallic;
            }

            perceptualRoughness = clamp(perceptualRoughness, c_MinRoughness, 1.0);
            metallic = clamp(metallic, 0.0, 1.0);
            // Roughness is authored as perceptual roughness; as is convention,
            // convert to material roughness by squaring the perceptual roughness [2].
            float alphaRoughness = perceptualRoughness * perceptualRoughness;

            // The albedo may be defined from a base texture or a flat color
            vec4 baseColor = u_BaseColorFactor;
            if (u_hasBaseColorSampler == 1) {
                vec2 uvTransformed = transformedUv(
                    u_BaseColorCoord,
                    u_BaseColorTransformScale,
                    u_BaseColorTransformOffset,
                    u_BaseColorTransformRotation
                );
                baseColor = SRGBtoLINEAR(texture2D(u_BaseColorSampler, uvTransformed)) * u_BaseColorFactor;
            }

            baseColor = vec4(v_Color, 1.0) * baseColor;

            vec3 f0 = vec3(0.04);
            vec3 diffuseColor = baseColor.rgb * (vec3(1.0) - f0);
            diffuseColor *= 1.0 - metallic;
            vec3 specularColor = mix(f0, baseColor.rgb, metallic);

            // Compute reflectance.
            float reflectance = max(max(specularColor.r, specularColor.g), specularColor.b);

            // For typical incident reflectance range (between 4% to 100%) set the grazing reflectance to 100% for typical fresnel effect.
            // For very low reflectance range on highly diffuse objects (below 4%), incrementally reduce grazing reflecance to 0%.
            float reflectance90 = clamp(reflectance * 25.0, 0.0, 1.0);
            vec3 specularEnvironmentR0 = specularColor.rgb;
            vec3 specularEnvironmentR90 = vec3(1.0, 1.0, 1.0) * reflectance90;

            vec3 n = getNormal();                             // normal at surface point
            vec3 v = normalize(u_Camera - v_Position);        // Vector from surface point to camera
            vec3 l = normalize(u_LightDirection);             // Vector from surface point to light
            vec3 h = normalize(l+v);                          // Half vector between both l and v
            vec3 reflection = -normalize(reflect(v, n));

            float NdotL = clamp(dot(n, l), 0.001, 1.0);
            float NdotV = clamp(abs(dot(n, v)), 0.001, 1.0);
            float NdotH = clamp(dot(n, h), 0.0, 1.0);
            float LdotH = clamp(dot(l, h), 0.0, 1.0);
            float VdotH = clamp(dot(v, h), 0.0, 1.0);

            PBRInfo pbrInputs = PBRInfo(
                NdotL,
                NdotV,
                NdotH,
                LdotH,
                VdotH,
                perceptualRoughness,
                metallic,
                specularEnvironmentR0,
                specularEnvironmentR90,
                alphaRoughness,
                diffuseColor,
                specularColor
            );


            // Calculate the shading terms for the microfacet specular shading model
            vec3 F = specularReflection(pbrInputs);
            float G = geometricOcclusion(pbrInputs);
            float D = microfacetDistribution(pbrInputs);

            // Calculation of analytical lighting contribution
            vec3 diffuseContrib = (1.0 - F) * diffuse(pbrInputs);
            vec3 specContrib = F * G * D / (4.0 * NdotL * NdotV);
            // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
            vec3 color = NdotL * v_Color * u_LightColor * (diffuseContrib + specContrib);


            // Calculate lighting contribution from image based lighting source (IBL)
            color += getIBLContribution(pbrInputs, n, reflection);

            if (u_hasOcclusionSampler == 1) {
                vec2 uvTransformed = transformedUv(
                    u_OcclusionCoord,
                    u_OcclusionTransformScale,
                    u_OcclusionTransformOffset,
                    u_OcclusionTransformRotation
                );
                float ao = texture2D(u_OcclusionSampler, uvTransformed).r;
                color = mix(color, color * ao, u_OcclusionStrength);
            }

            if (u_hasEmissiveSampler == 1) {
                vec2 uvTransformed = transformedUv(
                    u_EmissiveCoord,
                    u_EmissiveTransformScale,
                    u_EmissiveTransformOffset,
                    u_EmissiveTransformRotation
                );
                vec3 emissive = SRGBtoLINEAR(texture2D(u_EmissiveSampler, uvTransformed)).rgb * u_EmissiveFactor;
                color += emissive;
            }

            if (u_AlphaCutoff >= 0.0) {
                if (baseColor.a <= u_AlphaCutoff)
                {
                    discard;
                }
                baseColor.a = 1.0;
            }
            if (u_AlphaCutoff == -2.0) {
                baseColor.a = 1.0;
            }

            gl_FragColor = vec4(pow(color,vec3(1.0/2.2)), baseColor.a);
        }
    |]
