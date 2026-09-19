port module AnimationBenchmarks exposing (main)

import Base64
import Benchmark exposing (Benchmark)
import Benchmark.Runner.Cli
import BenchmarkGltf.Animation
import BenchmarkGltf.Query.AnimationHelper as BAnimationHelper
import BenchmarkGltf.Query.BufferStore as BBufferStore
import BenchmarkInternal.Gltf
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Gltf.Animation
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.BufferStore as BufferStore
import Internal.Gltf
import Json.Decode as JD


port sendOutput : Benchmark.Runner.Cli.Output -> Cmd msg


main : Program () Benchmark.Runner.Cli.Model Benchmark.Runner.Cli.Msg
main =
    Benchmark.Runner.Cli.program
        { suite = suite
        , sendOutput = sendOutput
        }


suite : Benchmark
suite =
    Benchmark.describe "Gltf.Animation"
        [ Benchmark.describe "single node animated with two channels"
            [ Benchmark.describe "animatedProperties" (List.map keyframes [ 16, 64, 256, 1024 ])
            ]
        ]


theta : Float
theta =
    0.5


keyframes : Int -> Benchmark
keyframes k =
    let
        json : String
        json =
            genGltfJson k

        current : List Gltf.Animation.Animation
        current =
            case JD.decodeString (Internal.Gltf.decoder "") json of
                Ok gltf ->
                    AnimationHelper.extractAnimations gltf (BufferStore.init gltf)

                Err _ ->
                    []

        benchmark : List BenchmarkGltf.Animation.Animation
        benchmark =
            case JD.decodeString (BenchmarkInternal.Gltf.decoder "") json of
                Ok gltf ->
                    BAnimationHelper.extractAnimations gltf (BBufferStore.init gltf)

                Err _ ->
                    []
    in
    Benchmark.compare (String.fromInt k ++ " keyframes")
        "current"
        (\_ -> Gltf.Animation.animatedProperties theta current)
        "benchmark"
        (\_ -> BenchmarkGltf.Animation.animatedProperties theta benchmark)


genGltfJson : Int -> String
genGltfJson k =
    let
        base64 : String
        base64 =
            bufferBytes k
                |> Base64.fromBytes
                |> Maybe.withDefault ""

        inputBytes : Int
        inputBytes =
            k * 4

        translationBytes : Int
        translationBytes =
            k * 3 * 4

        rotationBytes : Int
        rotationBytes =
            k * 4 * 4

        totalBytes : Int
        totalBytes =
            inputBytes + translationBytes + rotationBytes
    in
    """
{
  "asset": { "version": "2.0" },
  "scenes": [ { "nodes": [ 0 ] } ],
  "nodes": [ { "name": "node" } ],
  "meshes": [],
  "animations": [
    {
      "samplers": [
        { "input": 0, "output": 1, "interpolation": "LINEAR" },
        { "input": 0, "output": 2, "interpolation": "LINEAR" }
      ],
      "channels": [
        { "sampler": 0, "target": { "node": 0, "path": "translation" } },
        { "sampler": 1, "target": { "node": 0, "path": "rotation" } }
      ]
    }
  ],
  "accessors": [
    { "bufferView": 0, "componentType": 5126, "count": """ ++ String.fromInt k ++ """, "type": "SCALAR" },
    { "bufferView": 1, "componentType": 5126, "count": """ ++ String.fromInt k ++ """, "type": "VEC3" },
    { "bufferView": 2, "componentType": 5126, "count": """ ++ String.fromInt k ++ """, "type": "VEC4" }
  ],
  "bufferViews": [
    { "buffer": 0, "byteOffset": 0, "byteLength": """ ++ String.fromInt inputBytes ++ """ },
    { "buffer": 0, "byteOffset": """ ++ String.fromInt inputBytes ++ """, "byteLength": """ ++ String.fromInt translationBytes ++ """ },
    { "buffer": 0, "byteOffset": """ ++ String.fromInt (inputBytes + translationBytes) ++ """, "byteLength": """ ++ String.fromInt rotationBytes ++ """ }
  ],
  "buffers": [
    { "byteLength": """ ++ String.fromInt totalBytes ++ """, "uri": "data:application/octet-stream;base64,""" ++ base64 ++ """" }
  ]
}
"""


bufferBytes : Int -> Bytes
bufferBytes k =
    let
        indices : List Int
        indices =
            List.range 0 (k - 1)

        input : List Encode.Encoder
        input =
            List.map (\i -> Encode.float32 Bytes.LE (toFloat i)) indices

        translation : List Encode.Encoder
        translation =
            indices
                |> List.concatMap
                    (\i ->
                        [ Encode.float32 Bytes.LE (toFloat i)
                        , Encode.float32 Bytes.LE (toFloat i * 0.5)
                        , Encode.float32 Bytes.LE (toFloat i * -0.25)
                        ]
                    )

        rotation : List Encode.Encoder
        rotation =
            indices
                |> List.concatMap
                    (\_ ->
                        [ Encode.float32 Bytes.LE 0
                        , Encode.float32 Bytes.LE 0
                        , Encode.float32 Bytes.LE 0
                        , Encode.float32 Bytes.LE 1
                        ]
                    )
    in
    Encode.encode (Encode.sequence (input ++ translation ++ rotation))
