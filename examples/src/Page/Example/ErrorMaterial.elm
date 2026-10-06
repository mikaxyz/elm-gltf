module Page.Example.ErrorMaterial exposing (Config, renderer)

import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Entity, Shader)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms as Scene exposing (Uniforms)


type alias Uniforms =
    { sceneCamera : Mat4
    , scenePerspective : Mat4
    , sceneMatrix : Mat4
    , iResolution : Vec2
    , iTime : Float
    }


type alias Config =
    { resolution : Vec2
    , time : Float
    }


type alias Varyings =
    {}


renderer : Config -> Material.Options -> Scene.Uniforms u -> Object objectId materialId -> Entity
renderer config _ uniforms object =
    material
        { sceneCamera = uniforms.sceneCamera
        , scenePerspective = uniforms.scenePerspective
        , sceneMatrix = uniforms.sceneMatrix
        , iResolution = config.resolution
        , iTime = config.time
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
        precision lowp float;
        attribute vec3 position;
        uniform mat4 scenePerspective;
        uniform mat4 sceneCamera;
        uniform mat4 sceneMatrix;

        void main () {
            gl_Position = scenePerspective * sceneCamera * sceneMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision lowp float;
        uniform vec2 iResolution;
        uniform float iTime;
        const float PHI = 1.61803398874989484820459;

        float gold_noise(in vec2 xy, in float seed)
        {
            return fract(tan(distance(xy*PHI, xy)*seed)*xy.x);
        }

        void main () {
            // Credit: Gold Noise Uniform Random Static, https://www.shadertoy.com/view/ltB3zD
            vec2 xy = floor(gl_FragCoord.xy / 2.0);
            float seed = fract(iTime);
            gl_FragColor = vec4 (gold_noise(xy + iResolution.x, seed+0.1),
                                 gold_noise(xy + iResolution.x, seed+0.2),
                                 gold_noise(xy + iResolution.x, seed+0.3),
                                 gold_noise(xy + iResolution.x, seed+0.4));
        }
    |]
