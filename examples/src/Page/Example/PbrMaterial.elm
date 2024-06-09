module Page.Example.PbrMaterial exposing (renderer)

import Gltf.Query.ResolvedMaterial
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Shader)
import WebGL.Texture exposing (Texture)
import XYZMika.XYZ.Data.Vertex exposing (Vertex)
import XYZMika.XYZ.Material as Material exposing (Material)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Light.DirectionalLight as DirectionalLight
import XYZMika.XYZ.Scene.Light.PointLight as PointLight
import XYZMika.XYZ.Scene.Object exposing (BoneTransforms, Object)
import XYZMika.XYZ.Scene.Uniforms as Scene


type alias Uniforms =
    { u_MVPMatrix : Mat4
    , u_ModelMatrix : Mat4
    , u_NormalMatrix : Mat4
    , u_Camera : Vec3
    , u_LightDirection : Vec3
    , u_LightColor : Vec3
    , u_MetallicRoughnessValues : Vec2

    --
    , u_hasBaseColorSampler : Bool
    , u_BaseColorSampler : Texture
    , u_BaseColorFactor : Vec4

    --
    , u_hasNormalSampler : Bool
    , u_NormalSampler : Texture
    , u_NormalScale : Float

    --
    , u_hasOcclusionSampler : Bool
    , u_OcclusionSampler : Texture
    , u_OcclusionStrength : Float

    --
    , u_hasEmissiveSampler : Bool
    , u_EmissiveSampler : Texture
    , u_EmissiveFactor : Vec3

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
    }


type alias Varyings =
    { v_Position : Vec3
    , v_UV : Vec2

    --, v_TBN : Vec2
    , v_Normal : Vec3
    }


renderer :
    { fallbackTexture : Texture
    , environmentTexture : Texture
    , specularEnvironmentTexture : Texture
    , brdfLUTTexture : WebGL.Texture.Texture
    , camera : Camera
    }
    -> Gltf.Query.ResolvedMaterial.Material
    -> Material.Options
    -> Scene.Uniforms u
    -> Object objectId materialId
    -> Entity
renderer config (Gltf.Query.ResolvedMaterial.Material pbr) options uniforms object =
    let
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

        directionalLight3 : Vec3
        directionalLight3 =
            options
                |> Material.directionalLights
                |> List.head
                |> Maybe.map DirectionalLight.direction
                |> Maybe.withDefault (vec3 0 0 0)
    in
    material
        { u_MVPMatrix = Mat4.mul (Mat4.mul uniforms.scenePerspective uniforms.sceneCamera) uniforms.sceneMatrix
        , u_ModelMatrix = uniforms.sceneMatrix
        , u_NormalMatrix = uniforms.sceneRotationMatrix
        , u_Camera = Camera.position config.camera

        --
        , u_LightDirection = directionalLight3
        , u_LightColor = vec3 1 0 0
        , u_MetallicRoughnessValues = vec2 0 0.5

        --, u_BaseColorFactor = pbr.pbrMetallicRoughness.baseColorFactor
        , u_BaseColorFactor = vec4 1 1 1 1
        , u_hasBaseColorSampler = pbr.pbrMetallicRoughness.baseColorTexture /= Nothing
        , u_BaseColorSampler = pbr.pbrMetallicRoughness.baseColorTexture |> Maybe.withDefault config.fallbackTexture

        --
        , u_hasNormalSampler = pbr.normalTexture /= Nothing
        , u_NormalSampler = pbr.normalTexture |> Maybe.withDefault config.fallbackTexture
        , u_NormalScale = 1.0

        --
        , u_hasOcclusionSampler = pbr.occlusionTexture /= Nothing
        , u_OcclusionSampler = pbr.occlusionTexture |> Maybe.withDefault config.fallbackTexture
        , u_OcclusionStrength = 1.0

        --
        , u_hasEmissiveSampler = pbr.emissiveTexture /= Nothing
        , u_EmissiveSampler = pbr.emissiveTexture |> Maybe.withDefault config.fallbackTexture
        , u_EmissiveFactor = pbr.emissiveFactor

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
        precision highp float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 tangent;
        attribute vec2 uv;

        uniform mat4 u_MVPMatrix;
        uniform mat4 u_ModelMatrix;
        uniform mat4 u_NormalMatrix;

        varying vec3 v_Position;
        varying vec2 v_UV;
//        varying mat3 v_TBN;
        varying vec3 v_Normal;

        void main () {
            vec4 pos = u_ModelMatrix * vec4(position, 1.0);
            v_Position = vec3(pos.xyz) / pos.w;

//            #ifdef HAS_NORMALS
//            #ifdef HAS_TANGENTS
//            vec3 normalW = normalize(vec3(u_NormalMatrix * vec4(normal.xyz, 0.0)));
//            vec3 tangentW = normalize(vec3(u_ModelMatrix * vec4(tangent.xyz, 0.0)));
//            vec3 bitangentW = cross(normalW, tangentW) * tangent.w;
//            v_TBN = mat3(tangentW, bitangentW, normalW);
//            #else // HAS_TANGENTS != 1
            v_Normal = normalize(vec3(u_ModelMatrix * vec4(normal.xyz, 0.0)));
//            #endif
//            #endif

//            #ifdef HAS_UV
            v_UV = uv;
//            #else
//            v_UV = vec2(0.,0.);
//            #endif

            gl_Position = u_MVPMatrix * vec4(position, 1.0); // needs w for proper perspective correction
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision highp float;

        uniform vec3 u_LightDirection;
        uniform vec3 u_LightColor;
        uniform vec2 u_MetallicRoughnessValues;

        uniform sampler2D u_brdfLUT;
        uniform samplerCube u_DiffuseEnvSampler;
        uniform samplerCube u_SpecularEnvSampler;

        uniform vec4 u_BaseColorFactor;
        uniform bool u_hasBaseColorSampler;
        uniform sampler2D u_BaseColorSampler;

        uniform bool u_hasNormalSampler;
        uniform sampler2D u_NormalSampler;
        uniform float u_NormalScale;

        uniform bool u_hasOcclusionSampler;
        uniform sampler2D u_OcclusionSampler;
        uniform float u_OcclusionStrength;

        uniform bool u_hasEmissiveSampler;
        uniform sampler2D u_EmissiveSampler;
        uniform vec3 u_EmissiveFactor;

        uniform vec3 u_Camera;

        varying vec3 v_Position;
        varying vec2 v_UV;
//        varying mat3 v_TBN;
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
//            #ifdef MANUAL_SRGB
//                #ifdef SRGB_FAST_APPROXIMATION
//                    vec3 linOut = pow(srgbIn.xyz,vec3(2.2));
//                #else //SRGB_FAST_APPROXIMATION
//                    vec3 bLess = step(vec3(0.04045),srgbIn.xyz);
//                    vec3 linOut = mix( srgbIn.xyz/vec3(12.92), pow((srgbIn.xyz+vec3(0.055))/vec3(1.055),vec3(2.4)), bLess );
//                #endif //SRGB_FAST_APPROXIMATION
//            return vec4(linOut,srgbIn.w);;
//            #else //MANUAL_SRGB
//                return srgbIn;
//            #endif //MANUAL_SRGB


//            return srgbIn;

            vec3 linOut = pow(srgbIn.xyz,vec3(2.2));
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
//        vec3 f_environmentReflection (vec3 normal) {
//            vec3 dir = v_fragPos;
////            dir = normalize((sceneMatrix * vec4(v_fragPos, 1.0)).xyz);
//
//
//            vec3 normal_ = vec3(sceneRotationMatrix * vec4(v_normal, 0.0));
//
//            dir = normalize((scenePerspective * sceneCamera * vec4(v_fragPos,0.0)).xyz);
//
//            vec3 dirn = normalize((sceneMatrix * vec4(normal_,0.0)).xyz);
//
//            float intensity = dot(dirn, dir);
////            intensity = 1.0;
//
//            vec3 environment = textureCube(environmentTexture, dir).xyz;
//
//            return environment  * intensity * 0.1;
//        }

        // Find the normal for this fragment, pulling either from a predefined normal map
        // or from the interpolated mesh normal and tangent attributes.
        vec3 getNormal()
        {
//            // Retrieve the tangent space matrix
//            #ifndef HAS_TANGENTS
//                vec3 pos_dx = dFdx(v_Position);
//                vec3 pos_dy = dFdy(v_Position);
//                vec3 tex_dx = dFdx(vec3(v_UV, 0.0));
//                vec3 tex_dy = dFdy(vec3(v_UV, 0.0));
//                vec3 t = (tex_dy.t * pos_dx - tex_dx.t * pos_dy) / (tex_dx.s * tex_dy.t - tex_dy.s * tex_dx.t);
//
//                #ifdef HAS_NORMALS
//                    vec3 ng = normalize(v_Normal);
//                #else
//                    vec3 ng = cross(pos_dx, pos_dy);
//                #endif
//
//                t = normalize(t - ng * dot(ng, t));
//                vec3 b = normalize(cross(ng, t));
//                mat3 tbn = mat3(t, b, ng);
//            #else // HAS_TANGENTS
//                mat3 tbn = v_TBN;
//            #endif
//
//            #ifdef HAS_NORMALMAP
//                vec3 n = texture2D(u_NormalSampler, v_UV).rgb;
//                n = normalize(tbn * ((2.0 * n - 1.0) * vec3(u_NormalScale, u_NormalScale, 1.0)));
//            #else
//                // The tbn matrix is linearly interpolated, so we need to re-normalize
//                vec3 n = normalize(tbn[2].xyz);
//            #endif




            vec3 pos_dx = dFdx(v_Position);
            vec3 pos_dy = dFdy(v_Position);
            vec3 tex_dx = dFdx(vec3(v_UV, 0.0));
            vec3 tex_dy = dFdy(vec3(v_UV, 0.0));
            vec3 t = (tex_dy.t * pos_dx - tex_dx.t * pos_dy) / (tex_dx.s * tex_dy.t - tex_dy.s * tex_dx.t);


            vec3 ng = normalize(v_Normal);


            t = normalize(t - ng * dot(ng, t));
            vec3 b = normalize(cross(ng, t));
            mat3 tbn = mat3(t, b, ng);


            vec3 n;
            if (u_hasNormalSampler) {
                n = texture2D(u_NormalSampler, v_UV).rgb;
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
//        #ifdef USE_IBL
        vec3 getIBLContribution(PBRInfo pbrInputs, vec3 n, vec3 reflection)
        {
            float mipCount = 9.0; // resolution of 512x512
            float lod = (pbrInputs.perceptualRoughness * mipCount);
            // retrieve a scale and bias to F0. See [1], Figure 3
            vec3 brdf = SRGBtoLINEAR(texture2D(u_brdfLUT, vec2(pbrInputs.NdotV, 1.0 - pbrInputs.perceptualRoughness))).rgb;
            vec3 diffuseLight = SRGBtoLINEAR(textureCube(u_DiffuseEnvSampler, n)).rgb;

//            #ifdef USE_TEX_LOD
//            vec3 specularLight = SRGBtoLINEAR(textureCubeLodEXT(u_SpecularEnvSampler, reflection, lod)).rgb;
//            #else
            vec3 specularLight = SRGBtoLINEAR(textureCube(u_SpecularEnvSampler, reflection)).rgb;
//            #endif

//            vec3 specularLight = vec3(1.0);

            vec3 diffuse = diffuseLight * pbrInputs.diffuseColor;
//            vec3 specular = specularLight * (pbrInputs.specularColor * brdf.x + brdf.y);
            vec3 specular = specularLight * (pbrInputs.specularColor * brdf.x + brdf.y);

            // For presentation, this allows us to disable IBL terms
//            diffuse *= u_ScaleIBLAmbient.x;
//            specular *= u_ScaleIBLAmbient.y;

//            return diffuseLight + specular;
            return diffuse + specular;
        }
//        #endif







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
//            vec3 normal;
//            if(hasNormalTexture) {
//                normal = texture2D(normalTexture, v_uv).xyz;
//            } else {
//                normal = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));
//            }
//
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
//
//
//            vec3 baseColor = vec3(1);
//            if(pbrHasBaseColorTexture) {
//                baseColor = (texture2D(pbrBaseColorTexture, v_uv) * pbrBaseColorFactor).rgb;
//            }
//
//            vec3 n = vec3(sceneRotationMatrix * vec4(v_normal, 1.0));
//            vec3 environment = f_environmentReflection(n);
//
////            gl_FragColor =  vec4(environment, 1.0);
//
//            gl_FragColor =  vec4(lighting * v_color * baseColor, 1.0);


//            vec3 color = NdotL * u_LightColor * (diffuseContrib + specContrib);


//            gl_FragColor = vec4(pow(color,vec3(1.0/2.2)), baseColor.a);






            // Metallic and Roughness material properties are packed together
            // In glTF, these factors can be specified by fixed scalar values
            // or from a metallic-roughness map
            float perceptualRoughness = u_MetallicRoughnessValues.y;
            float metallic = u_MetallicRoughnessValues.x;
//            #ifdef HAS_METALROUGHNESSMAP
//            // Roughness is stored in the 'g' channel, metallic is stored in the 'b' channel.
//            // This layout intentionally reserves the 'r' channel for (optional) occlusion map data
//            vec4 mrSample = texture2D(u_MetallicRoughnessSampler, v_UV);
//            perceptualRoughness = mrSample.g * perceptualRoughness;
//            metallic = mrSample.b * metallic;
//            #endif
            perceptualRoughness = clamp(perceptualRoughness, c_MinRoughness, 1.0);
            metallic = clamp(metallic, 0.0, 1.0);
            // Roughness is authored as perceptual roughness; as is convention,
            // convert to material roughness by squaring the perceptual roughness [2].
            float alphaRoughness = perceptualRoughness * perceptualRoughness;

//    // The albedo may be defined from a base texture or a flat color
//    #ifdef HAS_BASECOLORMAP
//    vec4 baseColor = SRGBtoLINEAR(texture2D(u_BaseColorSampler, v_UV)) * u_BaseColorFactor;
//    #else
//    vec4 baseColor = u_BaseColorFactor;
//    #endif
            vec4 baseColor = u_BaseColorFactor;
            if (u_hasBaseColorSampler) {
                baseColor = SRGBtoLINEAR(texture2D(u_BaseColorSampler, v_UV)) * u_BaseColorFactor;
            }

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
            vec3 color = NdotL * u_LightColor * (diffuseContrib + specContrib);


//    // Calculate lighting contribution from image based lighting source (IBL)
//    #ifdef USE_IBL
//    color += getIBLContribution(pbrInputs, n, reflection);
            color += getIBLContribution(pbrInputs, n, reflection);
//    #endif

//    // Apply optional PBR terms for additional (optional) shading
//    #ifdef HAS_OCCLUSIONMAP
//    float ao = texture2D(u_OcclusionSampler, v_UV).r;
//    color = mix(color, color * ao, u_OcclusionStrength);
//    #endif
//
//    #ifdef HAS_EMISSIVEMAP
//    vec3 emissive = SRGBtoLINEAR(texture2D(u_EmissiveSampler, v_UV)).rgb * u_EmissiveFactor;
//    color += emissive;
//    #endif

            if (u_hasOcclusionSampler) {
                float ao = texture2D(u_OcclusionSampler, v_UV).r;
                color = mix(color, color * ao, u_OcclusionStrength);
            }

            if (u_hasEmissiveSampler) {
                vec3 emissive = SRGBtoLINEAR(texture2D(u_EmissiveSampler, v_UV)).rgb * u_EmissiveFactor;
                color += emissive;
            }

            float f_specularReflection = 0.5;
            float f_geometricOcclusion = 0.5;
            float f_microfacetDistribution = 0.5;
            float f_specContrib = 0.5;
            float f_diffuseContrib = 0.2;
            float f_baseColor = 0.2;
            float f_metallic = 0.5;
            float f_perceptualRoughness = 0.1;

//            color = mix(color, F, f_specularReflection);
//            color = mix(color, vec3(G), f_geometricOcclusion);
//            color = mix(color, vec3(D), f_microfacetDistribution);
//            color = mix(color, specContrib, f_specContrib);

//            color = mix(color, diffuseContrib, f_diffuseContrib);
//            color = mix(color, baseColor.rgb, f_baseColor);
//            color = mix(color, vec3(metallic), f_metallic);
//            color = mix(color, vec3(perceptualRoughness), f_perceptualRoughness);

            gl_FragColor = vec4(pow(color,vec3(1.0/2.2)), baseColor.a);
//            gl_FragColor = vec4(getIBLContribution(pbrInputs, n, reflection), 1.0);
        }
    |]
