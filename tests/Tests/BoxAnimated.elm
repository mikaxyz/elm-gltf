module Tests.BoxAnimated exposing (suite)

import Expect
import Gltf.Animation.Animation exposing (Animation(..))
import Gltf.Animation.Channel exposing (Channel(..))
import Gltf.Animation.Sampler exposing (Sampler(..))
import Gltf.Query.AnimationHelper as AnimationHelper
import Gltf.Query.BufferStore exposing (BufferStore)
import Internal.Gltf exposing (Gltf)
import Json.Decode as JD
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
    describe "BoxAnimated Example Asset"
        [ test "BufferStore initializes loaded buffers from gltf" <|
            \_ -> Expect.equal [] (Gltf.Query.BufferStore.getItemsToLoad gltf bufferStore)
        , describe "BoxAnimated"
            [ test "extract animations" <|
                \_ ->
                    let
                        animations : List Animation
                        animations =
                            AnimationHelper.extractAnimations gltf bufferStore
                    in
                    Expect.equal 1 (List.length animations)
            , test "animation sampler decodes inputMin/inputMax" <|
                \_ ->
                    let
                        samplers : List Sampler
                        samplers =
                            AnimationHelper.extractAnimations gltf bufferStore
                                |> List.concatMap (\(Animation animation) -> animation.channels)
                                |> List.map (\(Channel channel) -> channel.sampler)

                        minMaxInputs : List ( Float, Float )
                        minMaxInputs =
                            samplers |> List.map (\(Sampler sampler) -> ( sampler.inputMin, sampler.inputMax ))
                    in
                    Expect.equal [ ( 1.25, 2.5 ), ( 0, 3.708329916000366 ) ] minMaxInputs
            , test "animations contains startTime/endTime with range from all samplers" <|
                \_ ->
                    let
                        animationsMinMaxInputs : List ( Float, Float )
                        animationsMinMaxInputs =
                            AnimationHelper.extractAnimations gltf bufferStore
                                |> List.map (\(Animation animation) -> ( animation.startTime, animation.endTime ))
                    in
                    Expect.equal [ ( 0, 3.708329916000366 ) ] animationsMinMaxInputs
            ]
        ]


json : String
json =
    """
{
    "asset": {
        "generator": "COLLADA2GLTF",
        "version": "2.0"
    },
    "scene": 0,
    "scenes": [
        {
            "nodes": [
                3,
                0
            ]
        }
    ],
    "nodes": [
        {
            "children": [
                1
            ],
            "rotation": [
                -0.0,
                -0.0,
                -0.0,
                -1.0
            ]
        },
        {
            "children": [
                2
            ]
        },
        {
            "mesh": 0,
            "rotation": [
                -0.0,
                -0.0,
                -0.0,
                -1.0
            ]
        },
        {
            "mesh": 1
        }
    ],
    "meshes": [
        {
            "primitives": [
                {
                    "attributes": {
                        "NORMAL": 1,
                        "POSITION": 2
                    },
                    "indices": 0,
                    "mode": 4,
                    "material": 0
                }
            ],
            "name": "inner_box"
        },
        {
            "primitives": [
                {
                    "attributes": {
                        "NORMAL": 4,
                        "POSITION": 5
                    },
                    "indices": 3,
                    "mode": 4,
                    "material": 1
                }
            ],
            "name": "outer_box"
        }
    ],
    "animations": [
        {
            "channels": [
                {
                    "sampler": 0,
                    "target": {
                        "node": 2,
                        "path": "rotation"
                    }
                },
                {
                    "sampler": 1,
                    "target": {
                        "node": 0,
                        "path": "translation"
                    }
                }
            ],
            "samplers": [
                {
                    "input": 6,
                    "interpolation": "LINEAR",
                    "output": 7
                },
                {
                    "input": 8,
                    "interpolation": "LINEAR",
                    "output": 9
                }
            ]
        }
    ],
    "accessors": [
        {
            "bufferView": 0,
            "byteOffset": 0,
            "componentType": 5123,
            "count": 186,
            "max": [
                95
            ],
            "min": [
                0
            ],
            "type": "SCALAR"
        },
        {
            "bufferView": 1,
            "byteOffset": 0,
            "componentType": 5126,
            "count": 96,
            "max": [
                1.0,
                1.0,
                1.0
            ],
            "min": [
                -1.0,
                -1.0,
                -1.0
            ],
            "type": "VEC3"
        },
        {
            "bufferView": 1,
            "byteOffset": 1152,
            "componentType": 5126,
            "count": 96,
            "max": [
                0.33504000306129458,
                0.5,
                0.33504000306129458
            ],
            "min": [
                -0.33504000306129458,
                -0.5,
                -0.33504000306129458
            ],
            "type": "VEC3"
        },
        {
            "bufferView": 0,
            "byteOffset": 372,
            "componentType": 5123,
            "count": 576,
            "max": [
                223
            ],
            "min": [
                0
            ],
            "type": "SCALAR"
        },
        {
            "bufferView": 1,
            "byteOffset": 2304,
            "componentType": 5126,
            "count": 224,
            "max": [
                1.0,
                1.0,
                1.0
            ],
            "min": [
                -1.0,
                -1.0,
                -1.0
            ],
            "type": "VEC3"
        },
        {
            "bufferView": 1,
            "byteOffset": 4992,
            "componentType": 5126,
            "count": 224,
            "max": [
                0.5,
                0.5,
                0.5
            ],
            "min": [
                -0.5,
                -0.5,
                -0.5
            ],
            "type": "VEC3"
        },
        {
            "bufferView": 2,
            "byteOffset": 0,
            "componentType": 5126,
            "count": 2,
            "max": [
                2.5
            ],
            "min": [
                1.25
            ],
            "type": "SCALAR"
        },
        {
            "bufferView": 3,
            "byteOffset": 0,
            "componentType": 5126,
            "count": 2,
            "max": [
                1.0,
                0.0,
                0.0,
                4.4896593387466768e-11
            ],
            "min": [
                -0.0,
                0.0,
                0.0,
                -1.0
            ],
            "type": "VEC4"
        },
        {
            "bufferView": 2,
            "byteOffset": 8,
            "componentType": 5126,
            "count": 4,
            "max": [
                3.708329916000366
            ],
            "min": [
                0.0
            ],
            "type": "SCALAR"
        },
        {
            "bufferView": 4,
            "byteOffset": 0,
            "componentType": 5126,
            "count": 4,
            "max": [
                0.0,
                2.5199999809265138,
                0.0
            ],
            "min": [
                0.0,
                0.0,
                0.0
            ],
            "type": "VEC3"
        }
    ],
    "materials": [
        {
            "pbrMetallicRoughness": {
                "baseColorFactor": [
                    0.800000011920929,
                    0.4159420132637024,
                    0.7952920198440552,
                    1.0
                ],
                "metallicFactor": 0.0
            },
            "name": "inner"
        },
        {
            "pbrMetallicRoughness": {
                "baseColorFactor": [
                    0.3016040027141571,
                    0.5335419774055481,
                    0.800000011920929,
                    1.0
                ],
                "metallicFactor": 0.0
            },
            "name": "outer"
        }
    ],
    "bufferViews": [
        {
            "buffer": 0,
            "byteOffset": 7784,
            "byteLength": 1524,
            "target": 34963
        },
        {
            "buffer": 0,
            "byteOffset": 80,
            "byteLength": 7680,
            "byteStride": 12,
            "target": 34962
        },
        {
            "buffer": 0,
            "byteOffset": 7760,
            "byteLength": 24
        },
        {
            "buffer": 0,
            "byteOffset": 0,
            "byteLength": 32
        },
        {
            "buffer": 0,
            "byteOffset": 32,
            "byteLength": 48
        }
    ],
    "buffers": [
        {
            "byteLength": 9308,
            "uri": "data:application/octet-stream;base64,AAAAgAAAAIAAAACAAACAvwAAgD8AAAAAAAAAABJ1RS4AAAAAAAAAAAAAAAAAAAAArkchQAAAAAAAAAAArkchQAAAAAAAAAAAAAAAAAAAAAAAAAAA9wQ1v/cENT8AAAAA9wQ1v/cENT8AAAAA9wQ1v/cENT8AAAAA9wQ1v/cENT/3BDW/AAAAgPcENb/3BDW/AAAAgPcENb/3BDW/AAAAgPcENb/3BDW/AAAAgPcENb/3BDU/9wQ1vwAAAAD3BDU/9wQ1vwAAAAD3BDU/9wQ1vwAAAAD3BDU/9wQ1vwAAAAD3BDW/9wQ1vwAAAID3BDW/9wQ1vwAAAID3BDW/9wQ1vwAAAID3BDW/9wQ1vwAAAIAAAACA9wQ1v/cENb8AAACA9wQ1v/cENb8AAACA9wQ1v/cENb8AAACA9wQ1v/cENb8AAACA9wQ1P/cENT8AAACA9wQ1P/cENT8AAACA9wQ1P/cENT8AAACA9wQ1P/cENT/3BDU/9wQ1PwAAAAD3BDU/9wQ1PwAAAAD3BDU/9wQ1PwAAAAD3BDU/9wQ1PwAAAAD3BDW/9wQ1PwAAAAD3BDW/9wQ1PwAAAAD3BDW/9wQ1PwAAAAD3BDW/9wQ1PwAAAAD3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT8AAAAA9wQ1P/cENb8AAAAA9wQ1P/cENb8AAAAA9wQ1P/cENb8AAAAA9wQ1P/cENb/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb82zRO/Ns0TvzbNE782zRO/Ns0TvzbNE782zRO/Ns0TvzbNE782zRM/Ns0TvzbNE782zRM/Ns0TvzbNE782zRM/Ns0TvzbNE782zRM/Ns0TvzbNEz82zRM/Ns0TvzbNEz82zRM/Ns0TvzbNEz82zRO/Ns0TvzbNEz82zRO/Ns0TvzbNEz82zRO/Ns0TvzbNEz82zRO/Ns0TPzbNE782zRO/Ns0TPzbNE782zRO/Ns0TPzbNE782zRM/Ns0TPzbNE782zRM/Ns0TPzbNE782zRM/Ns0TPzbNE782zRM/Ns0TPzbNEz82zRM/Ns0TPzbNEz82zRM/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u7gtv7cIPD8AAAAAu7gtv7cIPD8AAAAAu7gtP7cIPL8AAAAAu7gtP7cIPL8AAAAAu7gtP7cIPL8AAAAAu7gtP7cIPL/RBVG/aM0TvwAAAIDRBVG/aM0TvwAAAIDRBVG/aM0TvwAAAIDRBVG/aM0TvwAAAIDRBVE/aM0TvwAAAADRBVE/aM0TvwAAAADRBVE/aM0TvwAAAADRBVE/aM0TvwAAAAAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIC3CDy/u7gtPwAAAAC3CDy/u7gtPwAAAAC3CDy/u7gtPwAAAAC3CDy/u7gtPwAAAAC3CDw/u7gtPwAAAAC3CDw/u7gtPwAAAAC3CDw/u7gtPwAAAAC3CDw/u7gtPwAAAAAAAAAAaM0Tv9EFUb8AAAAAaM0Tv9EFUb8AAAAAaM0Tv9EFUb8AAAAAaM0Tv9EFUb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb8AAAAAaM0TP9EFUT8AAAAAaM0TP9EFUT8AAAAAaM0TP9EFUT8AAAAAaM0TP9EFUT8AAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAACAu7gtP7cIPD8AAACAu7gtP7cIPD8AAACAu7gtP7cIPD8AAACAu7gtP7cIPD/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/RBVE/aM0TPwAAAIDRBVE/aM0TPwAAAIDRBVE/aM0TPwAAAIDRBVE/aM0TPwAAAIDRBVG/aM0TPwAAAADRBVG/aM0TPwAAAADRBVG/aM0TPwAAAADRBVG/aM0TPwAAAAAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAIAAAAAAAACAvwAAAID3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT8AAAAAaM0TP9EFUb8AAAAAaM0TP9EFUb8AAAAAaM0TP9EFUb8AAAAAaM0TP9EFUb+9//++GAU1v73//769//++GAU1v73//769//++GAU1v73//769//++GAU1v73//769//8+GAU1v73//769//8+GAU1v73//769//8+GAU1v73//769//8+GAU1v73//769//8+GAU1v73//z69//8+GAU1v73//z69//8+GAU1v73//z69//8+GAU1v73//z69//++GAU1v73//z69//++GAU1v73//z69//++GAU1v73//z69//++GAU1v73//z69//++GAU1P73//769//++GAU1P73//769//++GAU1P73//769//++GAU1P73//769//8+GAU1P73//769//8+GAU1P73//769//8+GAU1P73//769//8+GAU1P73//769//8+GAU1P73//z69//8+GAU1P73//z69//8+GAU1P73//z69//8+GAU1P73//z69//++GAU1P73//z69//++GAU1P73//z69//++GAU1P73//z69//++GAU1P73//z4/xvQ+lZ88vz/G9D4/xvQ+lZ88vz/G9D4/xvQ+lZ88vz/G9D4/xvQ+lZ88vz/G9D4/xvS+lZ88vz/G9D4/xvS+lZ88vz/G9D4/xvS+lZ88vz/G9D4/xvS+lZ88vz/G9D4/xvS+lZ88vz/G9L4/xvS+lZ88vz/G9L4/xvS+lZ88vz/G9L4/xvS+lZ88vz/G9L4/xvQ+lZ88vz/G9L4/xvQ+lZ88vz/G9L4/xvQ+lZ88vz/G9L4/xvQ+lZ88vz/G9L4/xvQ+lZ88Pz/G9D4/xvQ+lZ88Pz/G9D4/xvQ+lZ88Pz/G9D4/xvQ+lZ88Pz/G9D4/xvS+lZ88Pz/G9D4/xvS+lZ88Pz/G9D4/xvS+lZ88Pz/G9D4/xvS+lZ88Pz/G9D4/xvS+lZ88Pz/G9L4/xvS+lZ88Pz/G9L4/xvS+lZ88Pz/G9L4/xvS+lZ88Pz/G9L4/xvQ+lZ88Pz/G9L4/xvQ+lZ88Pz/G9L4/xvQ+lZ88Pz/G9L4/xvQ+lZ88Pz/G9L4AAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAACAAAAAAAAAgD8AAACAAAAAAAAAgD8AAACAAAAAAAAAgD8AAACAAAAAAAAAgD8AAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAACAAAAAgAAAgL8AAACAAAAAgAAAgL8AAACAAAAAgAAAgL8AAACAAAAAgAAAgL8AAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAACAAAAAgAAAgL8AAACAAAAAgAAAgL8AAACAAAAAgAAAgL8AAACAAAAAgAAAgL/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT/3BDW/AAAAAPcENT8AAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAACAAAAAAAAAgD8AAACAAAAAAAAAgD8AAACAAAAAAAAAgD8AAACAAAAAAAAAgD/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/3BDW/AAAAAPcENb/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENT/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/3BDU/AAAAAPcENb/pJrG+yXb+viGwsj7pJrE+yXb+viGwsj6oGbI+AAAAv3YbtD6oGbK+AAAAv3YbtD52G7S+AAAAv6gZsr7/6f6+AAAAv8hg/b7IYP2+AAAAv//p/r6oGbK+AAAAv3YbtL4hsLK+yXb+Pukmsb4hsLK+yXb+vukmsb7pJrG+yXb+viGwsr7pJrG+yXb+PiGwsr4hsLI+yXb+vukmsT4hsLI+yXb+vukmsb52G7Q+AAAAv6gZsr52G7Q+AAAAv6gZsj52G7S+AAAAv6gZsj52G7S+AAAAv6gZsr4hsLK+yXb+vukmsb4hsLK+yXb+vukmsT7Jdv4+yXb+vgAAAD/Jdv6+yXb+vgAAAD/IYP2+AAAAv//p/j7IYP0+AAAAv//p/j7pJrE+yXb+viGwsr7pJrG+yXb+viGwsr6oGbK+AAAAv3YbtL6oGbI+AAAAv3YbtL7pJrE+yXb+PiGwsj7pJrG+yXb+PiGwsj6oGbK+AAAAP3YbtD6oGbI+AAAAP3YbtD4AAAC/yXb+vsl2/j4AAAC/yXb+vsl2/r7/6f6+AAAAv8hg/b7/6f6+AAAAv8hg/T4AAAA/yXb+vsl2/r4AAAA/yXb+vsl2/j7/6f4+AAAAv8hg/T7/6f4+AAAAv8hg/b6oGbK+AAAAv3YbtD7IYP2+AAAAv//p/j7/6f6+AAAAv8hg/T52G7S+AAAAv6gZsj4hsLI+yXb+Pukmsb4hsLI+yXb+PukmsT52G7Q+AAAAP6gZsj52G7Q+AAAAP6gZsr4hsLK+yXb+PukmsT4hsLK+yXb+Pukmsb52G7S+AAAAP6gZsr52G7S+AAAAP6gZsj7Jdv6+yXb+vgAAAL/Jdv4+yXb+vgAAAL/IYP0+AAAAv//p/r7IYP2+AAAAv//p/r7pJrG+yXb+PiGwsj7pJrG+yXb+viGwsj4hsLK+yXb+vukmsT4hsLK+yXb+PukmsT7IYP0+AAAAP//p/j7IYP2+AAAAP//p/j7Jdv6+yXb+PgAAAD/Jdv4+yXb+PgAAAD92G7Q+AAAAv6gZsj7/6f4+AAAAv8hg/T7IYP0+AAAAv//p/j6oGbI+AAAAv3YbtD7pJrG+yXb+PiGwsr7pJrE+yXb+PiGwsr6oGbI+AAAAP3YbtL6oGbK+AAAAP3YbtL7pJrE+yXb+viGwsj7pJrE+yXb+PiGwsj4hsLI+yXb+PukmsT4hsLI+yXb+vukmsT7/6f4+AAAAP8hg/b7/6f4+AAAAP8hg/T4AAAA/yXb+Psl2/j4AAAA/yXb+Psl2/r7/6f6+AAAAP8hg/T7/6f6+AAAAP8hg/b4AAAC/yXb+Psl2/r4AAAC/yXb+Psl2/j6oGbI+AAAAv3YbtL7IYP0+AAAAv//p/r7/6f4+AAAAv8hg/b52G7Q+AAAAv6gZsr4hsLI+yXb+vukmsb4hsLI+yXb+Pukmsb7pJrE+yXb+PiGwsr7pJrE+yXb+viGwsr7IYP2+AAAAP//p/r7IYP0+AAAAP//p/r7Jdv4+yXb+PgAAAL/Jdv6+yXb+PgAAAL/Jdv6+yXb+vgAAAL/IYP2+AAAAv//p/r7/6f6+AAAAv8hg/b4AAAC/yXb+vsl2/r4AAAA/yXb+vsl2/r7/6f4+AAAAv8hg/b7IYP0+AAAAv//p/r7Jdv4+yXb+vgAAAL/Jdv4+yXb+vgAAAD/IYP0+AAAAv//p/j7/6f4+AAAAv8hg/T4AAAA/yXb+vsl2/j4AAAC/yXb+vsl2/j7/6f6+AAAAv8hg/T7IYP2+AAAAv//p/j7Jdv6+yXb+vgAAAD8AAAC/yXb+Psl2/r7/6f6+AAAAP8hg/b7IYP2+AAAAP//p/r7Jdv6+yXb+PgAAAL/IYP0+AAAAP//p/r7/6f4+AAAAP8hg/b4AAAA/yXb+Psl2/r7Jdv4+yXb+PgAAAL//6f4+AAAAP8hg/T7IYP0+AAAAP//p/j7Jdv4+yXb+PgAAAD8AAAA/yXb+Psl2/j7IYP2+AAAAP//p/j7/6f6+AAAAP8hg/T4AAAC/yXb+Psl2/j7Jdv6+yXb+PgAAAD+oGbK+AAAAv3YbtL7pJrG+yXb+viGwsr4hsLK+yXb+vukmsb52G7S+AAAAv6gZsr52G7Q+AAAAv6gZsr4hsLI+yXb+vukmsb7pJrE+yXb+viGwsr6oGbI+AAAAv3YbtL6oGbI+AAAAv3YbtD7pJrE+yXb+viGwsj4hsLI+yXb+vukmsT52G7Q+AAAAv6gZsj52G7S+AAAAv6gZsj4hsLK+yXb+vukmsT7pJrG+yXb+viGwsj6oGbK+AAAAv3YbtD52G7S+AAAAP6gZsr4hsLK+yXb+Pukmsb7pJrG+yXb+PiGwsr6oGbK+AAAAP3YbtL6oGbI+AAAAP3YbtL7pJrE+yXb+PiGwsr4hsLI+yXb+Pukmsb52G7Q+AAAAP6gZsr52G7Q+AAAAP6gZsj4hsLI+yXb+PukmsT7pJrE+yXb+PiGwsj6oGbI+AAAAP3YbtD6oGbK+AAAAP3YbtD7pJrG+yXb+PiGwsj4hsLK+yXb+PukmsT52G7S+AAAAP6gZsj4hsLK+yXb+PukmsT4hsLK+yXb+vukmsT4hsLK+yXb+vukmsb4hsLK+yXb+Pukmsb7pJrG+yXb+PiGwsr7pJrG+yXb+viGwsr7pJrE+yXb+viGwsr7pJrE+yXb+PiGwsr4hsLI+yXb+Pukmsb4hsLI+yXb+vukmsb4hsLI+yXb+vukmsT4hsLI+yXb+PukmsT7pJrE+yXb+PiGwsj7pJrE+yXb+viGwsj7pJrG+yXb+viGwsj7pJrG+yXb+PiGwsj7/6f4+AAAAP8hg/b7IYP0+AAAAP//p/r6oGbI+AAAAP3YbtL52G7Q+AAAAP6gZsr7/6f6+AAAAP8hg/b7/6f6+AAAAP8hg/T52G7S+AAAAP6gZsj52G7S+AAAAP6gZsr7/6f4+AAAAP8hg/T52G7Q+AAAAP6gZsj7IYP2+AAAAP//p/r6oGbK+AAAAP3YbtL7IYP0+AAAAP//p/j6oGbI+AAAAP3YbtD7IYP2+AAAAP//p/j6oGbK+AAAAP3YbtD4AAAC/yXb+Psl2/j4AAAC/yXb+Psl2/r4AAAC/yXb+vsl2/r4AAAC/yXb+vsl2/j7Jdv6+yXb+PgAAAL/Jdv4+yXb+PgAAAL/Jdv4+yXb+vgAAAL/Jdv6+yXb+vgAAAL/Jdv6+yXb+PgAAAD8AAAC/yXb+Psl2/j4AAAC/yXb+vsl2/j7Jdv6+yXb+vgAAAD8AAAA/yXb+Psl2/r4AAAA/yXb+Psl2/j4AAAA/yXb+vsl2/j4AAAA/yXb+vsl2/r7Jdv4+yXb+PgAAAD/Jdv6+yXb+PgAAAD/Jdv6+yXb+vgAAAD/Jdv4+yXb+vgAAAD8AAAC/yXb+Psl2/r7Jdv6+yXb+PgAAAL/Jdv6+yXb+vgAAAL8AAAC/yXb+vsl2/r4AAAA/yXb+Psl2/j7Jdv4+yXb+PgAAAD/Jdv4+yXb+vgAAAD8AAAA/yXb+vsl2/j7Jdv4+yXb+PgAAAL8AAAA/yXb+Psl2/r4AAAA/yXb+vsl2/r7Jdv4+yXb+vgAAAL8AAKA/AAAgQAAAAAAAAKA/AAAgQEdVbUAAAAEAAgABAAIAAwACAAMAAAAEAAUABgAFAAYABwAGAAcABAAIAAkACgAJAAoACwAKAAsACAAMAA0ADgANAA4ADwAOAA8ADAAQABEAEgARABIAEwASABMAEAAUABUAFgAVABYAFwAWABcAFAAYABkAGgAZABoAGwAaABsAGAAcAB0AHgAdAB4AHwAeAB8AHAAgACEAIgAhACIAIwAiACMAIAAkACUAJgAlACYAJwAmACcAJAAoACkAKgApACoAKwAqACsAKAAsAC0ALgAtAC4ALwAuAC8ALAAwADEAMgAzADQANQA2ADcAOAA5ADoAOwA8AD0APgA/AEAAQQBCAEMARABFAEYARwBIAEkASgBJAEoASwBKAEsASABMAE0ATgBNAE4ATwBOAE8ATABQAFEAUgBRAFIAUwBSAFMAUABUAFUAVgBVAFYAVwBWAFcAVABYAFkAWgBZAFoAWwBaAFsAWABcAF0AXgBdAF4AXwBeAF8AXAAAAAEAAgABAAIAAwACAAMAAAAEAAUABgAFAAYABwAGAAcABAAIAAkACgAJAAoACwAKAAsACAAMAA0ADgANAA4ADwAOAA8ADAAQABEAEgARABIAEwASABMAEAAUABUAFgAVABYAFwAWABcAFAAYABkAGgAZABoAGwAaABsAGAAcAB0AHgAdAB4AHwAeAB8AHAAgACEAIgAhACIAIwAiACMAIAAkACUAJgAlACYAJwAmACcAJAAoACkAKgApACoAKwAqACsAKAAsAC0ALgAtAC4ALwAuAC8ALAAwADEAMgAxADIAMwAyADMAMAA0ADUANgA1ADYANwA2ADcANAA4ADkAOgA5ADoAOwA6ADsAOAA8AD0APgA9AD4APwA+AD8APABAAEEAQgBBAEIAQwBCAEMAQABEAEUARgBFAEYARwBGAEcARABIAEkASgBJAEoASwBKAEsASABMAE0ATgBNAE4ATwBOAE8ATABQAFEAUgBRAFIAUwBSAFMAUABUAFUAVgBVAFYAVwBWAFcAVABYAFkAWgBZAFoAWwBaAFsAWABcAF0AXgBdAF4AXwBeAF8AXABgAGEAYgBhAGIAYwBiAGMAYABkAGUAZgBlAGYAZwBmAGcAZABoAGkAagBpAGoAawBqAGsAaABsAG0AbgBtAG4AbwBuAG8AbABwAHEAcgBxAHIAcwByAHMAcAB0AHUAdgB1AHYAdwB2AHcAdAB4AHkAegB5AHoAewB6AHsAeAB8AH0AfgB9AH4AfwB+AH8AfACAAIEAggCBAIIAgwCCAIMAgACEAIUAhgCFAIYAhwCGAIcAhACIAIkAigCJAIoAiwCKAIsAiACMAI0AjgCNAI4AjwCOAI8AjACQAJEAkgCRAJIAkwCSAJMAkACUAJUAlgCVAJYAlwCWAJcAlACYAJkAmgCZAJoAmwCaAJsAmACcAJ0AngCdAJ4AnwCeAJ8AnAApACgAQwAoAEMAQgBDAEIAKQAFAAQAKwAEACsAKgArACoABQBVAFQABwBUAAcABgAHAAYAVQBBAEAAVwBAAFcAVgBXAFYAQQCgAKEAogChAKIAowCiAKMAoACkAKUApgClAKYApwCmAKcApACoAKkAqgCpAKoAqwCqAKsAqACsAK0ArgCtAK4ArwCuAK8ArACwALEAsgCxALIAswCyALMAsAC0ALUAtgC1ALYAtwC2ALcAtAC4ALAAswCwALMAuQCzALkAuAC6ALQAtwC0ALcAuwC3ALsAugC8ALgAuQC4ALkAvQC5AL0AvACxALoAuwC6ALsAsgC7ALIAsQC+ALwAvQC8AL0AvwC9AL8AvgC1AL4AvwC+AL8AtgC/ALYAtQDAAMEAwgDBAMIAwwDCAMMAwADEAMUAxgDFAMYAxwDGAMcAxADIAMkAygDJAMoAywDKAMsAyADMAM0AzgDNAM4AzwDOAM8AzADQANEA0gDRANIA0wDSANMA0ADUANUA1gDVANYA1wDWANcA1ADYANkA2gDZANoA2wDaANsA2ADcAN0A3gDdAN4A3wDeAN8A3AA="
        }
    ]
}
"""
