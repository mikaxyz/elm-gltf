module Tests.Gltf exposing (suite)

import Expect exposing (Expectation)
import Json.Decode as JD
import Test exposing (..)
import Xyz.Gltf.Raw.Gltf as Gltf exposing (Gltf)


suite : Test
suite =
    describe "Gltf Package"
        [ describe "Gltf Decoder"
            [ test "decodes SimpleTriangle" <|
                \_ ->
                    let
                        parsed =
                            JD.decodeString Gltf.decoder json
                    in
                    Expect.ok parsed
            , test "decodes asset" <|
                \_ ->
                    let
                        parsed =
                            JD.decodeString Gltf.decoder json
                                |> Result.map (\{ asset } -> asset.version)
                    in
                    Expect.equal (Ok "2.0") parsed
            ]
        ]


json =
    """
{
  "scene": 0,
  "scenes" : [
    {
      "nodes" : [ 0 ]
    }
  ],

  "nodes" : [
    {
      "mesh" : 0
    }
  ],

  "meshes" : [
    {
      "primitives" : [ {
        "attributes" : {
          "POSITION" : 1
        },
        "indices" : 0
      } ]
    }
  ],

  "buffers" : [
    {
      "uri" : "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA=",
      "byteLength" : 44
    }
  ],
  "bufferViews" : [
    {
      "buffer" : 0,
      "byteOffset" : 0,
      "byteLength" : 6,
      "target" : 34963
    },
    {
      "buffer" : 0,
      "byteOffset" : 8,
      "byteLength" : 36,
      "target" : 34962
    }
  ],
  "accessors" : [
    {
      "bufferView" : 0,
      "byteOffset" : 0,
      "componentType" : 5123,
      "count" : 3,
      "type" : "SCALAR",
      "max" : [ 2 ],
      "min" : [ 0 ]
    },
    {
      "bufferView" : 1,
      "byteOffset" : 0,
      "componentType" : 5126,
      "count" : 3,
      "type" : "VEC3",
      "max" : [ 1.0, 1.0, 0.0 ],
      "min" : [ 0.0, 0.0, 0.0 ]
    }
  ],

  "asset" : {
    "version" : "2.0"
  }
}
"""
