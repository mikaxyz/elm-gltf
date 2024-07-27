module Tests.Gtlf.Animation exposing (suite)

import Array
import Expect exposing (Expectation)
import Gltf.Animation.Animation exposing (Animation(..))
import Gltf.Animation.Channel exposing (Channel(..))
import Gltf.Animation.Sampler exposing (Sampler(..))
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.BufferStore as BufferStore
import Internal.Accessor as Accessor
import Internal.Animation
import Internal.Animation.Channel
import Internal.Animation.Sampler
import Internal.Gltf exposing (Gltf)
import Internal.Node as Node
import Json.Decode as JD
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Gltf Package: Animation"
        [ describe "Decoder"
            [ test "decodes ok" <|
                \_ ->
                    let
                        parsed : Result JD.Error Gltf
                        parsed =
                            JD.decodeString (Internal.Gltf.decoder "test") json
                    in
                    Expect.ok parsed
            , test "decodes animations" <|
                \_ ->
                    let
                        parsed : Result JD.Error (Array.Array Internal.Animation.Animation)
                        parsed =
                            JD.decodeString (Internal.Gltf.decoder "test") json
                                |> Result.map (\{ animations } -> animations)

                        expected : Array.Array Internal.Animation.Animation
                        expected =
                            Internal.Animation.Animation
                                { name = Just "CubeAction"
                                , channels =
                                    [ Internal.Animation.Channel.Channel
                                        { sampler = Internal.Animation.Sampler.Index 0
                                        , target =
                                            { node = Node.Index 0
                                            , path = Internal.Animation.Channel.Translation
                                            }
                                        }
                                    ]
                                        |> Array.fromList
                                , samplers =
                                    [ Internal.Animation.Sampler.Sampler
                                        { input = Accessor.Index 4
                                        , interpolation = Internal.Animation.Sampler.Linear
                                        , output = Accessor.Index 5
                                        }
                                    ]
                                        |> Array.fromList
                                }
                                |> List.singleton
                                |> Array.fromList
                    in
                    Expect.equal (Ok expected) parsed
            ]
        , describe "Gltf Extract"
            [ test "Extracts animations" <|
                \_ ->
                    let
                        extracted : Result JD.Error (List Animation)
                        extracted =
                            JD.decodeString (Internal.Gltf.decoder "test") json
                                |> Result.map (\x -> AnimationHelper.extractAnimations x (BufferStore.init x))

                        expectSampler : Sampler -> Expectation
                        expectSampler extractedSampler =
                            extractedSampler
                                |> Expect.all
                                    [ \(Sampler sampler) ->
                                        Expect.equal (List.length sampler.input) 62
                                    , \(Sampler sampler) ->
                                        Expect.equal (List.length sampler.output) 62
                                    , \(Sampler sampler) ->
                                        Expect.equal sampler.interpolation Gltf.Animation.Sampler.Linear
                                    ]

                        expectChannel : Channel -> Expectation
                        expectChannel extractedChannel =
                            extractedChannel
                                |> Expect.all
                                    [ \(Channel channel) ->
                                        expectSampler channel.sampler
                                    ]
                    in
                    case extracted of
                        Ok animations ->
                            Expect.all
                                [ \subject -> Expect.equal (List.length subject) 1
                                , \subject ->
                                    case subject of
                                        (Animation animation) :: [] ->
                                            Expect.equal (Array.length animation.samplers) 1

                                        _ ->
                                            Expect.fail "Expecting 1 sampler"
                                , \subject ->
                                    case subject of
                                        (Animation animation) :: [] ->
                                            case animation.samplers |> Array.toList of
                                                singleSampler :: [] ->
                                                    expectSampler singleSampler

                                                _ ->
                                                    Expect.fail "To many samplers"

                                        _ ->
                                            Expect.fail "To many animations"
                                , \subject ->
                                    case subject of
                                        (Animation animation) :: [] ->
                                            case animation.channels of
                                                singleChannel :: [] ->
                                                    expectChannel singleChannel

                                                channels ->
                                                    Expect.fail <| "Expecting 1 channel got " ++ String.fromInt (List.length channels)

                                        _ ->
                                            Expect.fail "To many animations"
                                ]
                                animations

                        Err _ ->
                            Expect.fail "Extract failed"
            ]
        ]


json : String
json =
    """
{
    "asset":{
        "copyright":"me@mika.xyz",
        "generator":"Khronos glTF Blender I/O v3.6.27",
        "version":"2.0"
    },
    "scene":0,
    "scenes":[
        {
            "name":"Scene",
            "nodes":[
                0
            ]
        }
    ],
    "nodes":[
        {
            "mesh":0,
            "name":"Cube"
        }
    ],
    "animations":[
        {
            "channels":[
                {
                    "sampler":0,
                    "target":{
                        "node":0,
                        "path":"translation"
                    }
                }
            ],
            "name":"CubeAction",
            "samplers":[
                {
                    "input":4,
                    "interpolation":"LINEAR",
                    "output":5
                }
            ]
        }
    ],
    "meshes":[
        {
            "name":"Cube",
            "primitives":[
                {
                    "attributes":{
                        "POSITION":0,
                        "NORMAL":1,
                        "TEXCOORD_0":2
                    },
                    "indices":3
                }
            ]
        }
    ],
    "accessors":[
        {
            "bufferView":0,
            "componentType":5126,
            "count":24,
            "max":[
                1,
                1,
                1
            ],
            "min":[
                -1,
                -1,
                -1
            ],
            "type":"VEC3"
        },
        {
            "bufferView":1,
            "componentType":5126,
            "count":24,
            "type":"VEC3"
        },
        {
            "bufferView":2,
            "componentType":5126,
            "count":24,
            "type":"VEC2"
        },
        {
            "bufferView":3,
            "componentType":5123,
            "count":36,
            "type":"SCALAR"
        },
        {
            "bufferView":4,
            "componentType":5126,
            "count":62,
            "max":[
                2.5416666666666665
            ],
            "min":[
                0
            ],
            "type":"SCALAR"
        },
        {
            "bufferView":5,
            "componentType":5126,
            "count":62,
            "type":"VEC3"
        }
    ],
    "bufferViews":[
        {
            "buffer":0,
            "byteLength":288,
            "byteOffset":0,
            "target":34962
        },
        {
            "buffer":0,
            "byteLength":288,
            "byteOffset":288,
            "target":34962
        },
        {
            "buffer":0,
            "byteLength":192,
            "byteOffset":576,
            "target":34962
        },
        {
            "buffer":0,
            "byteLength":72,
            "byteOffset":768,
            "target":34963
        },
        {
            "buffer":0,
            "byteLength":248,
            "byteOffset":840
        },
        {
            "buffer":0,
            "byteLength":744,
            "byteOffset":1088
        }
    ],
    "buffers":[
        {
            "byteLength":1832,
            "uri":"data:application/octet-stream;base64,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"
        }
    ]
}
"""
