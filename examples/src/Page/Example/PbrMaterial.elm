module Page.Example.PbrMaterial exposing (renderer)

import Gltf.Query.ResolvedMaterial
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight
import XYZMika.XYZ.Scene.Light.PointLight as PointLight
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)
import XYZMika.XYZ.Scene.Uniforms as Scene


type alias Uniforms =
    { sceneCamera : Mat4
    , scenePerspective : Mat4
    , sceneMatrix : Mat4
    , sceneRotationMatrix : Mat4

    --
    , pbrBaseColorFactor : Vec4
    , pbrHasBaseColorTexture : Bool
    , pbrBaseColorTexture : Texture

    --
    , hasNormalTexture : Bool
    , normalTexture : Texture

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
    }


type alias Varyings =
    { v_color : Vec3
    , v_normal : Vec3
    , v_uv : Vec2
    , v_fragPos : Vec3
    }


renderer : Texture -> Gltf.Query.ResolvedMaterial.Material -> Material.Options -> Scene.Uniforms u -> Object objectId materialId -> Entity
renderer fallbackTexture (Gltf.Query.ResolvedMaterial.Material pbr) options uniforms object =
    let
        boneTransforms : BoneTransforms
        boneTransforms =
            Object.boneTransforms object
                |> Maybe.withDefault Object.boneTransformsIdentity

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
    in
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , sceneRotationMatrix = uniforms.sceneRotationMatrix

        --
        , pbrBaseColorFactor = pbr.pbrMetallicRoughness.baseColorFactor
        , pbrHasBaseColorTexture = pbr.pbrMetallicRoughness.baseColorTexture /= Nothing
        , pbrBaseColorTexture = pbr.pbrMetallicRoughness.baseColorTexture |> Maybe.withDefault fallbackTexture

        --
        , hasNormalTexture = pbr.normalTexture /= Nothing
        , normalTexture = pbr.normalTexture |> Maybe.withDefault fallbackTexture

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
        }
        |> Material.toEntity object


material : Uniforms -> Material Uniforms Varyings
material uniforms =
    Material.material
        uniforms
        vertexShader
        fragmentShader


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 color;
        attribute vec3 normal;
        attribute vec4 joints;
        attribute vec4 weights;
        attribute vec2 uv;

        uniform mat4 sceneCamera;
        uniform mat4 scenePerspective;
        uniform mat4 sceneMatrix;
        uniform mat4 sceneRotationMatrix;

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

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        mat4 jointMat(int i) {
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
            return m;
        }

        void main () {
            mat4 skinDeform =
                jointMat(int(joints.x)) * weights.x +
                jointMat(int(joints.y)) * weights.y +
                jointMat(int(joints.z)) * weights.z +
                jointMat(int(joints.w)) * weights.w;

            gl_Position = scenePerspective * sceneCamera * sceneMatrix * skinDeform * vec4(position, 1.0);
            v_fragPos = vec3(sceneMatrix * vec4(position, 1.0));
            v_uv = uv;
            v_color = color;

            v_normal = normal;
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 v_color;
        varying vec3 v_normal;
        varying vec2 v_uv;
        varying vec3 v_fragPos;

        uniform mat4 sceneRotationMatrix;

        uniform vec4 directionalLight;
        uniform vec4 pointLight1;
        uniform vec3 pointLight1Color;
        uniform vec4 pointLight2;
        uniform vec3 pointLight2Color;
        uniform vec4 pointLight3;
        uniform vec3 pointLight3Color;
        uniform vec4 pointLight4;
        uniform vec3 pointLight4Color;
        uniform vec4 pointLight5;
        uniform vec3 pointLight5Color;

        uniform bool pbrHasBaseColorTexture;
        uniform vec4 pbrBaseColorFactor;
        uniform sampler2D pbrBaseColorTexture;
        uniform bool hasNormalTexture;
        uniform sampler2D normalTexture;

        vec3 f_pointLight (vec3 normal, vec3 lightPosition) {
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 pointLightDirection = normalize(lightPosition - v_fragPos);
            highp float pointLightDiff = max(dot(normal, pointLightDirection), 0.0);
            highp float intensity = pow(pointLightDiff, 1.0);
            highp float distance = distance(lightPosition, v_fragPos);

            return color * intensity;
        }

        vec3 f_directionalLight (vec3 normal) {
            highp vec3 color = vec3(1.0, 1.0, 1.0);
            highp vec3 directionalVector = normalize(directionalLight.xyz);
            highp float intensity = max(dot(normal, directionalVector), 0.0);
            return color * intensity;
        }

        void main () {
            vec3 normal;
            if(hasNormalTexture) {
                normal = texture2D(normalTexture, v_uv).xyz;
            } else {
                normal = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));
            }

            vec3 lighting = vec3(0);

            if (pointLight1.w > 0.0) {
               lighting += pointLight1.w * f_pointLight(normal, pointLight1.xyz) * pointLight1Color;
            }
            if (pointLight2.w > 0.0) {
               lighting += pointLight2.w * f_pointLight(normal, pointLight2.xyz) * pointLight2Color;
            }
            if (pointLight3.w > 0.0) {
               lighting += pointLight3.w * f_pointLight(normal, pointLight3.xyz) * pointLight3Color;
            }
            if (pointLight4.w > 0.0) {
               lighting += pointLight4.w * f_pointLight(normal, pointLight4.xyz) * pointLight4Color;
            }
            if (pointLight5.w > 0.0) {
               lighting += pointLight5.w * f_pointLight(normal, pointLight5.xyz) * pointLight5Color;
            }

            if (directionalLight.w > 0.0) {
                lighting += f_directionalLight(normal);
            }


            vec3 baseColor = vec3(1);
            if(pbrHasBaseColorTexture) {
                baseColor = (texture2D(pbrBaseColorTexture, v_uv) * pbrBaseColorFactor).rgb;
            }

            gl_FragColor =  vec4(lighting * v_color * baseColor, 1.0);

        }
    |]
