module Tests.MeshPrimitiveModes exposing (suite)

import Common
import Expect
import Gltf.Mesh exposing (Mesh)
import Gltf.Query.BufferStore exposing (BufferStore)
import Gltf.Query.MeshHelper
import Internal.Gltf exposing (Gltf)
import Internal.Mesh
import Json.Decode as JD
import Math.Vector3 as Vec3
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
    describe "Parse meshes in MeshPrimitiveModes Example Asset"
        [ test "BufferStore initializes loaded buffers from gltf" <|
            \_ -> Expect.equal [] (Gltf.Query.BufferStore.getItemsToLoad gltf bufferStore)
        , describe "decodes meshes"
            -- https://github.com/KhronosGroup/glTF-Sample-Assets/blob/main/Models/MeshPrimitiveModes/README.md#structure
            [ test "decodes Points" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 0)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List Gltf.Mesh.Vertex
                        expectedVertices =
                            [ v0, v1, v2, v3, v4, v5, v6 ]
                    in
                    Expect.equal (Just (Gltf.Mesh.Points Nothing expectedVertices)) mesh
            , test "decodes Lines" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 1)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List ( Gltf.Mesh.Vertex, Gltf.Mesh.Vertex )
                        expectedVertices =
                            [ ( v0, v1 )
                            , ( v0, v2 )
                            , ( v0, v3 )
                            , ( v0, v4 )
                            , ( v0, v5 )
                            , ( v0, v6 )
                            ]
                    in
                    Expect.equal (Just (Gltf.Mesh.Lines Nothing expectedVertices)) mesh
            , test "decodes LineLoop" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 2)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List Gltf.Mesh.Vertex
                        expectedVertices =
                            [ v0, v1, v2, v3, v4, v5, v6 ]
                    in
                    Expect.equal (Just (Gltf.Mesh.LineLoop Nothing expectedVertices)) mesh
            , test "decodes LineStrip" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 3)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List Gltf.Mesh.Vertex
                        expectedVertices =
                            [ v0, v1, v2, v3, v4, v5, v6 ]
                    in
                    Expect.equal (Just (Gltf.Mesh.LineStrip Nothing expectedVertices)) mesh
            , test "decodes TriangleStrip" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 5)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List Gltf.Mesh.Vertex
                        expectedVertices =
                            [ v2
                            , v3
                            , v1
                            , v4
                            , v6
                            , v5
                            ]
                    in
                    Expect.equal (Just (Gltf.Mesh.TriangleStrip Nothing expectedVertices)) mesh
            , test "decodes TriangleFan" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 6)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : List Gltf.Mesh.Vertex
                        expectedVertices =
                            [ v0
                            , v1
                            , v2
                            , v3
                            , v4
                            , v5
                            , v6
                            , v1
                            ]
                    in
                    Expect.equal (Just (Gltf.Mesh.TriangleFan Nothing expectedVertices)) mesh
            , test "decodes IndexedTriangularMesh" <|
                \_ ->
                    let
                        mesh : Maybe Mesh
                        mesh =
                            Common.meshAtIndex gltf (Internal.Mesh.Index 4)
                                |> Maybe.map .primitives
                                |> Maybe.andThen List.head
                                |> Maybe.map (Gltf.Query.MeshHelper.fromPrimitive gltf bufferStore)

                        expectedVertices : ( List Gltf.Mesh.Vertex, List ( Int, Int, Int ) )
                        expectedVertices =
                            ( [ v0, v1, v2, v3, v4, v5, v6 ]
                            , [ ( 0, 1, 2 )
                              , ( 0, 2, 3 )
                              , ( 0, 3, 4 )
                              , ( 0, 4, 5 )
                              , ( 0, 5, 6 )
                              , ( 0, 6, 1 )
                              ]
                            )
                    in
                    Expect.equal (Just (Gltf.Mesh.IndexedTriangularMesh Nothing expectedVertices)) mesh
            ]
        ]


v0 : Gltf.Mesh.Vertex
v0 =
    vertex { x = 0, y = 0, z = 0 }


v1 : Gltf.Mesh.Vertex
v1 =
    vertex { x = 0.8659999966621399, y = -0.5, z = 0 }


v2 : Gltf.Mesh.Vertex
v2 =
    vertex { x = 0.8659999966621399, y = 0.5, z = 0 }


v3 : Gltf.Mesh.Vertex
v3 =
    vertex { x = 0, y = 1, z = 0 }


v4 : Gltf.Mesh.Vertex
v4 =
    vertex { x = -0.8659999966621399, y = 0.5, z = 0 }


v5 : Gltf.Mesh.Vertex
v5 =
    vertex { x = -0.8659999966621399, y = -0.5, z = 0 }


v6 : Gltf.Mesh.Vertex
v6 =
    vertex { x = 0, y = -1, z = 0 }


vertex : { x : Float, y : Float, z : Float } -> Gltf.Mesh.Vertex
vertex { x, y, z } =
    { position = Vec3.vec3 x y z
    , normal = Nothing
    , tangent = Nothing
    , color = Nothing
    , weights = Nothing
    , joints = Nothing
    , texCoord0 = Nothing
    , texCoord1 = Nothing
    }


json : String
json =
    """
{
  "asset": {
    "version": "2.0"
  },

  "scene": 0,
  "scenes": [
    {
      "nodes": [0, 1, 2, 3, 4, 5, 6]
    }
  ],

  "nodes": [
    {
      "translation": [-0, 3, 0],
      "mesh": 0
    },
    {
      "translation": [-2, 0, 0],
      "mesh": 1
    },
    {
      "translation": [0, 0, 0],
      "mesh": 2
    },
    {
      "translation": [2, 0, 0],
      "mesh": 3
    },
    {
      "translation": [-2, -3, 0],
      "mesh": 4
    },
    {
      "translation": [0, -3, 0],
      "mesh": 5
    },
    {
      "translation": [2, -3, 0],
      "mesh": 6
    }
  ],

  "meshes": [
    {
      "name": "mesh with POINTS",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 0,
          "mode": 0
        }
      ]
    },
    {
      "name": "mesh with LINES",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 1,
          "mode": 1
        }
      ]
    },
    {
      "name": "mesh with LINE_LOOP",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 2,
          "mode": 2
        }
      ]
    },
    {
      "name": "mesh with LINE_STRIP",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 3,
          "mode": 3
        }
      ]
    },
    {
      "name": "mesh with TRIANGLES",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 4,
          "mode": 4
        }
      ]
    },
    {
      "name": "mesh with GL_TRIANGLE_STRIP",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 5,
          "mode": 5
        }
      ]
    },
    {
      "name": "mesh with GL_TRIANGLE_FAN",
      "primitives": [
        {
          "attributes": {
            "POSITION": 7
          },
          "indices": 6,
          "mode": 6
        }
      ]
    }
  ],

  "buffers": [
    {
      "uri" : "data:application/gltf-buffer;base64,AAABAAIAAwAEAAUABgAAAAEAAAACAAAAAwAAAAQAAAAFAAAABgAAAAEAAgADAAQABQAGAAAAAQACAAMABAAFAAYAAAABAAIAAAACAAMAAAADAAQAAAAEAAUAAAAFAAYAAAAGAAEAAgADAAEABAAGAAUAAAABAAIAAwAEAAUABgABAAAAAAAAAAAAAAAAAAAALbJdPwAAAL8AAAAALbJdPwAAAD8AAAAAAAAAAAAAgD8AAAAALbJdvwAAAD8AAAAALbJdvwAAAL8AAAAAAAAAAAAAgL8AAAAA",
      "byteLength": 216
    }
  ],
  "bufferViews": [
    {
      "buffer": 0,
      "byteOffset": 0,
      "byteLength": 130,
      "target": 34963
    },
    {
      "buffer": 0,
      "byteOffset": 132,
      "byteLength": 84,
      "target": 34962
    }
  ],
  "accessors": [
    {
      "name": "indices for POINTS",
      "bufferView": 0,
      "byteOffset": 0,
      "componentType": 5123,
      "count": 7,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "indices for LINES",
      "bufferView": 0,
      "byteOffset": 14,
      "componentType": 5123,
      "count": 12,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "indices for LINE_LOOP",
      "bufferView": 0,
      "byteOffset": 38,
      "componentType": 5123,
      "count": 7,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "indices for LINE_STRIP",
      "bufferView": 0,
      "byteOffset": 52,
      "componentType": 5123,
      "count": 7,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "indices for TRIANGLES",
      "bufferView": 0,
      "byteOffset": 66,
      "componentType": 5123,
      "count": 18,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "indices for TRIANGLE_STRIP",
      "bufferView": 0,
      "byteOffset": 102,
      "componentType": 5123,
      "count": 6,
      "type": "SCALAR",
      "max": [6],
      "min": [1]
    },
    {
      "name": "indices for TRIANGLE_FAN",
      "bufferView": 0,
      "byteOffset": 114,
      "componentType": 5123,
      "count": 8,
      "type": "SCALAR",
      "max": [6],
      "min": [0]
    },
    {
      "name": "positions",
      "bufferView": 1,
      "byteOffset": 0,
      "componentType": 5126,
      "count": 7,
      "type": "VEC3",
      "max": [0.866, 1.0, 0.0],
      "min": [-0.866, -1.0, 0.0]
    }
  ]
}
"""
