module Tests.AnimatedTriangle exposing (suite)

import Expect
import Gltf.Animation
import Gltf.Animation.Animation exposing (Animation)
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.BufferStore exposing (BufferStore)
import Internal.Gltf exposing (Gltf)
import Json.Decode as JD
import Quaternion exposing (Quaternion)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        gltf : Result JD.Error Gltf
        gltf =
            JD.decodeString (Internal.Gltf.decoder "test.json") json

        bufferStore : Result JD.Error BufferStore
        bufferStore =
            gltf |> Result.map Gltf.Query.BufferStore.init
    in
    case Result.map2 Tuple.pair gltf bufferStore of
        Ok ( gltf_, bufferStore_ ) ->
            tests gltf_ bufferStore_

        Err _ ->
            test "Parse meshes in MeshPrimitiveModes Example Asset" <| \_ -> Expect.fail "Failed to initialize test subjects"


tests : Gltf -> BufferStore -> Test
tests gltf bufferStore =
    describe "AnimatedTriangle Example Asset"
        [ test "BufferStore initializes loaded buffers from gltf" <|
            \_ -> Expect.equal [] (Gltf.Query.BufferStore.getItemsToLoad gltf bufferStore)
        , describe "AnimatedTriangle"
            [ test "extract animations" <|
                \_ ->
                    let
                        animations : List Animation
                        animations =
                            AnimationHelper.extractAnimations gltf bufferStore
                    in
                    Expect.equal (List.length animations) 1
            , test "gets animated properties at time 0" <|
                \_ ->
                    let
                        animations : List Animation
                        animations =
                            AnimationHelper.extractAnimations gltf bufferStore

                        animatedProperties : List Gltf.Animation.AnimatedProperty
                        animatedProperties =
                            Gltf.Animation.animatedProperties 0 animations

                        expected : List Gltf.Animation.AnimatedProperty
                        expected =
                            [ Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) Quaternion.identity
                            ]
                    in
                    Expect.equal expected animatedProperties
            , test "gets animated properties at keyframes" <|
                \_ ->
                    let
                        animations : List Animation
                        animations =
                            AnimationHelper.extractAnimations gltf bufferStore

                        animatedProperties : List Gltf.Animation.AnimatedProperty
                        animatedProperties =
                            [ 0.0, 0.25, 0.5, 0.75, 1.0 ]
                                |> List.concatMap (\t -> Gltf.Animation.animatedProperties t animations)

                        roundWithPrecision : Int -> Float -> Float
                        roundWithPrecision p v =
                            let
                                d : Float
                                d =
                                    toFloat (10 ^ p)
                            in
                            (round (v * d) |> toFloat) / d

                        roundQuaternion : Quaternion -> Quaternion
                        roundQuaternion q =
                            { vector =
                                { x = roundWithPrecision 15 q.vector.x
                                , y = roundWithPrecision 15 q.vector.y
                                , z = roundWithPrecision 15 q.vector.z
                                }
                            , scalar = roundWithPrecision 15 q.scalar
                            }

                        expected : List Gltf.Animation.AnimatedProperty
                        expected =
                            [ Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) Quaternion.identity
                            , Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) (Quaternion.quaternion 0.7070000171661377 0 0 0.7070000171661377)
                            , Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) (Quaternion.zRotation pi |> roundQuaternion)
                            , Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) (Quaternion.quaternion -0.7070000171661377 0 0 0.7070000171661377)
                            , Gltf.Animation.AnimatedRotationProperty (NodeIndex 0) Quaternion.identity
                            ]
                    in
                    Expect.equal expected animatedProperties
            ]
        ]


json : String
json =
    """
{
  "scene" : 0,
  "scenes" : [
    {
      "nodes" : [ 0 ]
    }
  ],

  "nodes" : [
    {
      "mesh" : 0,
      "rotation" : [ 0.0, 0.0, 0.0, 1.0 ]
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

  "animations": [
    {
      "samplers" : [
        {
          "input" : 2,
          "interpolation" : "LINEAR",
          "output" : 3
        }
      ],
      "channels" : [ {
        "sampler" : 0,
        "target" : {
          "node" : 0,
          "path" : "rotation"
        }
      } ]
    }
  ],

  "buffers" : [
    {
      "uri" : "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA=",
      "byteLength" : 44
    },
    {
      "uri" : "data:application/octet-stream;base64,AAAAAAAAgD4AAAA/AABAPwAAgD8AAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAD0/TQ/9P00PwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAPT9ND/0/TS/AAAAAAAAAAAAAAAAAACAPw==",
      "byteLength" : 100
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
    },
    {
      "buffer" : 1,
      "byteOffset" : 0,
      "byteLength" : 100
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
    },
    {
      "bufferView" : 2,
      "byteOffset" : 0,
      "componentType" : 5126,
      "count" : 5,
      "type" : "SCALAR",
      "max" : [ 1.0 ],
      "min" : [ 0.0 ]
    },
    {
      "bufferView" : 2,
      "byteOffset" : 20,
      "componentType" : 5126,
      "count" : 5,
      "type" : "VEC4",
      "max" : [ 0.0, 0.0, 1.0, 1.0 ],
      "min" : [ 0.0, 0.0, 0.0, -0.707 ]
    }
  ],

  "asset" : {
    "version" : "2.0"
  }

}
"""
