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
    "KHR_materials_dispersion": { "dispersion": 0.1 },
    "KHR_materials_ior": { "ior": 1.31 },
    "KHR_materials_transmission": {
      "transmissionFactor": 0.5,
      "transmissionTexture": { "index": 1 }
    },
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
    "KHR_materials_dispersion": {},
    "KHR_materials_ior": {},
    "KHR_materials_transmission": {},
    "KHR_materials_volume": {}
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
