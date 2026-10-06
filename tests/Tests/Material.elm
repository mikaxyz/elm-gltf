module Tests.Material exposing (suite)

import Expect
import Gltf.Material.Extensions as Extensions
import Internal.Material
import Internal.Texture
import Internal.TextureInfo exposing (TextureInfo)
import Json.Decode as JD
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Material decoder"
        [ test "Decodes material without extensions" <|
            \_ ->
                let
                    expected : Maybe Internal.Material.ExtensionsInfo
                    expected =
                        Nothing
                in
                JD.decodeString Internal.Material.decoder jsonWithoutExtensions
                    |> Result.map .extensions
                    |> Expect.equal (Ok expected)
        , test "Keeps raw extensions value" <|
            \_ ->
                JD.decodeString Internal.Material.decoder json
                    |> Result.map
                        (.extensions
                            >> Maybe.map (.raw >> JD.decodeValue (JD.at [ "KHR_materials_ior", "ior" ] JD.float))
                        )
                    |> Expect.equal (Ok (Just (Ok 1.31)))
        , describe "KHR_materials_anisotropy"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.AnisotropyExtensionInfo
                        expected =
                            { strength = 0.6
                            , rotation = 1.57
                            , texture = Just (TextureInfo (Internal.Texture.Index 3) 0 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .anisotropy)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.AnisotropyExtensionInfo
                        expected =
                            { strength = 0
                            , rotation = 0
                            , texture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .anisotropy)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_clearcoat"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.ClearcoatExtensionInfo
                        expected =
                            { factor = 1
                            , texture = Just (TextureInfo (Internal.Texture.Index 4) 0 Nothing)
                            , roughnessFactor = 0.3
                            , roughnessTexture = Just (TextureInfo (Internal.Texture.Index 5) 0 Nothing)
                            , normalTexture =
                                Just
                                    { index = Internal.Texture.Index 6
                                    , texCoord = 0
                                    , scale = 2
                                    , extensions = Nothing
                                    }
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .clearcoat)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.ClearcoatExtensionInfo
                        expected =
                            { factor = 0
                            , texture = Nothing
                            , roughnessFactor = 0
                            , roughnessTexture = Nothing
                            , normalTexture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .clearcoat)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_dispersion"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Extensions.Dispersion
                        expected =
                            Extensions.Dispersion 0.1
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .dispersion)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Extensions.Dispersion
                        expected =
                            Extensions.Dispersion 0
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .dispersion)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_emissive_strength"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Extensions.EmissiveStrength
                        expected =
                            Extensions.EmissiveStrength 5
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .emissiveStrength)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Extensions.EmissiveStrength
                        expected =
                            Extensions.EmissiveStrength 1
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .emissiveStrength)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_ior"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Extensions.Ior
                        expected =
                            Extensions.Ior 1.31
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .ior)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Extensions.Ior
                        expected =
                            Extensions.Ior 1.5
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .ior)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_iridescence"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.IridescenceExtensionInfo
                        expected =
                            { factor = 1
                            , texture = Just (TextureInfo (Internal.Texture.Index 7) 0 Nothing)
                            , ior = 1.8
                            , thicknessMinimum = 200
                            , thicknessMaximum = 800
                            , thicknessTexture = Just (TextureInfo (Internal.Texture.Index 8) 0 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .iridescence)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.IridescenceExtensionInfo
                        expected =
                            { factor = 0
                            , texture = Nothing
                            , ior = 1.3
                            , thicknessMinimum = 100
                            , thicknessMaximum = 400
                            , thicknessTexture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .iridescence)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_sheen"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.SheenExtensionInfo
                        expected =
                            { colorFactor = vec3 0.9 0.8 0.7
                            , colorTexture = Just (TextureInfo (Internal.Texture.Index 9) 0 Nothing)
                            , roughnessFactor = 0.4
                            , roughnessTexture = Just (TextureInfo (Internal.Texture.Index 10) 0 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .sheen)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.SheenExtensionInfo
                        expected =
                            { colorFactor = vec3 0 0 0
                            , colorTexture = Nothing
                            , roughnessFactor = 0
                            , roughnessTexture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .sheen)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_specular"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.SpecularExtensionInfo
                        expected =
                            { factor = 0.7
                            , texture = Just (TextureInfo (Internal.Texture.Index 11) 0 Nothing)
                            , colorFactor = vec3 0.1 0.2 0.3
                            , colorTexture = Just (TextureInfo (Internal.Texture.Index 12) 0 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .specular)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.SpecularExtensionInfo
                        expected =
                            { factor = 1
                            , texture = Nothing
                            , colorFactor = vec3 1 1 1
                            , colorTexture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .specular)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_transmission"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.TransmissionExtensionInfo
                        expected =
                            { factor = 0.5
                            , texture = Just (TextureInfo (Internal.Texture.Index 1) 0 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .transmission)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.TransmissionExtensionInfo
                        expected =
                            { factor = 0
                            , texture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .transmission)
                        |> Expect.equal (Just expected |> Ok)
            ]
        , describe "KHR_materials_unlit"
            [ test "Decodes presence" <|
                \_ ->
                    let
                        expected : Extensions.Unlit
                        expected =
                            Extensions.Unlit
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .unlit)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes absence" <|
                \_ ->
                    JD.decodeString Internal.Material.decoder jsonWithoutUnlit
                        |> Result.map (.extensions >> Maybe.andThen .unlit)
                        |> Expect.equal (Nothing |> Ok)
            ]
        , describe "KHR_materials_volume"
            [ test "Decodes values" <|
                \_ ->
                    let
                        expected : Internal.Material.VolumeExtensionInfo
                        expected =
                            { thicknessFactor = 0.2
                            , attenuationColor = vec3 0.4 0.5 0.6
                            , attenuationDistance = Just 0.006
                            , thicknessTexture = Just (TextureInfo (Internal.Texture.Index 2) 1 Nothing)
                            }
                    in
                    JD.decodeString Internal.Material.decoder json
                        |> Result.map (.extensions >> Maybe.andThen .volume)
                        |> Expect.equal (Just expected |> Ok)
            , test "Decodes empty to defaults" <|
                \_ ->
                    let
                        expected : Internal.Material.VolumeExtensionInfo
                        expected =
                            { thicknessFactor = 0
                            , attenuationColor = vec3 1 1 1
                            , attenuationDistance = Nothing
                            , thicknessTexture = Nothing
                            }
                    in
                    JD.decodeString Internal.Material.decoder jsonWithEmptyExtensions
                        |> Result.map (.extensions >> Maybe.andThen .volume)
                        |> Expect.equal (Just expected |> Ok)
            ]
        ]


json : String
json =
    """
{
  "name": "Glass",
  "extensions": {
    "KHR_materials_anisotropy": {
      "anisotropyStrength": 0.6,
      "anisotropyRotation": 1.57,
      "anisotropyTexture": { "index": 3 }
    },
    "KHR_materials_clearcoat": {
      "clearcoatFactor": 1,
      "clearcoatTexture": { "index": 4 },
      "clearcoatRoughnessFactor": 0.3,
      "clearcoatRoughnessTexture": { "index": 5 },
      "clearcoatNormalTexture": { "index": 6, "scale": 2 }
    },
    "KHR_materials_dispersion": { "dispersion": 0.1 },
    "KHR_materials_emissive_strength": { "emissiveStrength": 5 },
    "KHR_materials_ior": { "ior": 1.31 },
    "KHR_materials_iridescence": {
      "iridescenceFactor": 1,
      "iridescenceTexture": { "index": 7 },
      "iridescenceIor": 1.8,
      "iridescenceThicknessMinimum": 200,
      "iridescenceThicknessMaximum": 800,
      "iridescenceThicknessTexture": { "index": 8 }
    },
    "KHR_materials_sheen": {
      "sheenColorFactor": [0.9, 0.8, 0.7],
      "sheenColorTexture": { "index": 9 },
      "sheenRoughnessFactor": 0.4,
      "sheenRoughnessTexture": { "index": 10 }
    },
    "KHR_materials_specular": {
      "specularFactor": 0.7,
      "specularTexture": { "index": 11 },
      "specularColorFactor": [0.1, 0.2, 0.3],
      "specularColorTexture": { "index": 12 }
    },
    "KHR_materials_transmission": {
      "transmissionFactor": 0.5,
      "transmissionTexture": { "index": 1 }
    },
    "KHR_materials_unlit": {},
    "KHR_materials_volume": {
      "attenuationColor": [0.4, 0.5, 0.6],
      "attenuationDistance": 0.006,
      "thicknessFactor": 0.2,
      "thicknessTexture": { "index": 2, "texCoord": 1 }
    }
  }
}
"""


jsonWithEmptyExtensions : String
jsonWithEmptyExtensions =
    """
{
  "name": "Glass",
  "extensions": {
    "KHR_materials_anisotropy": {},
    "KHR_materials_clearcoat": {},
    "KHR_materials_dispersion": {},
    "KHR_materials_emissive_strength": {},
    "KHR_materials_ior": {},
    "KHR_materials_iridescence": {},
    "KHR_materials_sheen": {},
    "KHR_materials_specular": {},
    "KHR_materials_transmission": {},
    "KHR_materials_unlit": {},
    "KHR_materials_volume": {}
  }
}
"""


jsonWithoutUnlit : String
jsonWithoutUnlit =
    """
{
  "name": "Glass",
  "extensions": {
    "KHR_materials_ior": { "ior": 1.31 }
  }
}
"""


jsonWithoutExtensions : String
jsonWithoutExtensions =
    """
{
  "name": "Glass"
}
"""
