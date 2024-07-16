module Tests.Gtlf.Query exposing (suite)

import Expect
import Gltf.Query as Query
import Gltf.Query.Skin exposing (Skin(..))
import Gltf.Query.TriangularMesh exposing (TriangularMesh(..), Vertex)
import Internal.Mesh as Mesh
import Internal.Node as Node exposing (Node(..))
import Internal.Skin as GltfSkin
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 as Vec4
import Test exposing (Test, describe, test)
import Tree exposing (Tree)


suite : Test
suite =
    describe "Gltf Query"
        [ test "Extracts node" <|
            \_ ->
                let
                    queryResult : Result Query.Error (Tree Node)
                    queryResult =
                        Query.fromJson simpleSkin (Query.nodeTree 0)

                    expected : Node
                    expected =
                        Node
                            { index = Node.Index 0
                            , name = Nothing
                            , meshIndex = Just (Mesh.Index 0)
                            , skinIndex = Just (GltfSkin.Index 0)
                            , cameraIndex = Nothing
                            , children = []
                            , transform =
                                Node.RTS
                                    { rotation = Nothing
                                    , translation = Nothing
                                    , scale = Nothing
                                    }
                            }
                in
                case queryResult of
                    Ok tree ->
                        Expect.equal
                            (Tree.singleton expected)
                            tree

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts node with children" <|
            \_ ->
                let
                    queryResult : Result Query.Error (Tree Node.Index)
                    queryResult =
                        Query.fromJson simpleSkin (Query.nodeTree 1)
                            |> Result.map (Tree.map (\(Node node) -> node.index))

                    expected : Tree Node.Index
                    expected =
                        Tree.tree (Node.Index 1)
                            [ Tree.singleton (Node.Index 2) ]
                in
                case queryResult of
                    Ok tree ->
                        Expect.equal
                            expected
                            tree

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts scene node trees" <|
            \_ ->
                let
                    nodeToIndex : Node -> Node.Index
                    nodeToIndex (Node node) =
                        node.index

                    queryResult : Result Query.Error (List (Tree Node.Index))
                    queryResult =
                        Query.fromJson simpleSkin (Query.sceneNodeTrees 0)
                            |> Result.map (List.map (Tree.map nodeToIndex))

                    expected : List (Tree Node.Index)
                    expected =
                        [ Tree.singleton (Node.Index 0)
                        , Tree.tree (Node.Index 1)
                            [ Tree.singleton (Node.Index 2) ]
                        ]
                in
                case queryResult of
                    Ok trees ->
                        Expect.equal
                            expected
                            trees

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts simpleSkin mesh with correct number of indices and vertices" <|
            \_ ->
                let
                    queryResult : Result Query.Error Query.Node
                    queryResult =
                        Query.fromJson simpleSkin (Query.treeFromNode (Node.Index 0))
                            |> Result.map Tree.label
                in
                case queryResult of
                    Ok thing ->
                        Expect.all
                            [ \x ->
                                Expect.equal [ ( 10, 8 ) ]
                                    (Query.meshesFromNode x
                                        |> List.filterMap
                                            (\mesh ->
                                                case mesh of
                                                    IndexedTriangularMesh _ ( indices, vertices ) ->
                                                        Just ( List.length indices, List.length vertices )

                                                    _ ->
                                                        Nothing
                                            )
                                    )
                            ]
                            thing

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts simpleSkin mesh with skin" <|
            \_ ->
                let
                    -- EXPECTED DATA FROM:
                    -- https://github.khronos.org/glTF-Tutorials/gltfTutorial/gltfTutorial_020_Skins.html
                    expectedPositions : List Vec3.Vec3
                    expectedPositions =
                        [ vec3 -0.5 0.0 0.0
                        , vec3 0.5 0.0 0.0
                        , vec3 -0.5 0.5 0.0
                        , vec3 0.5 0.5 0.0
                        , vec3 -0.5 1.0 0.0
                        , vec3 0.5 1.0 0.0
                        , vec3 -0.5 1.5 0.0
                        , vec3 0.5 1.5 0.0
                        , vec3 -0.5 2.0 0.0
                        , vec3 0.5 2.0 0.0
                        ]

                    expectedWeights : List Vec4.Vec4
                    expectedWeights =
                        [ Vec4.vec4 1.0 0.0 0.0 0.0
                        , Vec4.vec4 1.0 0.0 0.0 0.0
                        , Vec4.vec4 0.75 0.25 0.0 0.0
                        , Vec4.vec4 0.75 0.25 0.0 0.0
                        , Vec4.vec4 0.5 0.5 0.0 0.0
                        , Vec4.vec4 0.5 0.5 0.0 0.0
                        , Vec4.vec4 0.25 0.75 0.0 0.0
                        , Vec4.vec4 0.25 0.75 0.0 0.0
                        , Vec4.vec4 0.0 1.0 0.0 0.0
                        , Vec4.vec4 0.0 1.0 0.0 0.0
                        ]

                    expectedJoints : List { j1 : number, j2 : number, j3 : number, j4 : number }
                    expectedJoints =
                        [ { j1 = 0, j2 = 0, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 0, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        , { j1 = 0, j2 = 1, j3 = 0, j4 = 0 }
                        ]

                    expectedIndices : List ( number, number, number )
                    expectedIndices =
                        [ ( 0, 1, 3 )
                        , ( 0, 3, 2 )
                        , ( 2, 3, 5 )
                        , ( 2, 5, 4 )
                        , ( 4, 5, 7 )
                        , ( 4, 7, 6 )
                        , ( 6, 7, 9 )
                        , ( 6, 9, 8 )
                        ]

                    expectedVertices : List Vertex
                    expectedVertices =
                        List.map3
                            (\position weights joints ->
                                { position = position
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , weights = Just weights
                                , joints = Just joints
                                , texCoords = Nothing
                                }
                            )
                            expectedPositions
                            expectedWeights
                            expectedJoints

                    expected : List TriangularMesh
                    expected =
                        [ IndexedTriangularMesh Nothing
                            ( expectedVertices
                            , expectedIndices
                            )
                        ]

                    queryResult : Result Query.Error Query.Node
                    queryResult =
                        Query.fromJson simpleSkin (Query.treeFromNode (Node.Index 0))
                            |> Result.map Tree.label
                in
                case queryResult of
                    Ok mesh ->
                        Expect.equal expected (Query.meshesFromNode mesh)

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts IndexedTriangularMesh from Triangle" <|
            \_ ->
                let
                    queryResult : Result Query.Error Query.Node
                    queryResult =
                        Query.fromJson triangle (Query.treeFromNode (Node.Index 0))
                            |> Result.map Tree.label

                    expected : List TriangularMesh
                    expected =
                        [ IndexedTriangularMesh Nothing
                            ( [ { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 0 0 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              , { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 1 0 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              , { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 0 1 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              ]
                            , [ ( 0, 1, 2 ) ]
                            )
                        ]
                in
                case queryResult of
                    Ok thing ->
                        Expect.all
                            [ \x ->
                                Expect.equal expected (Query.meshesFromNode x)
                            ]
                            thing

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts TriangularMesh from TriangleWithoutIndices" <|
            \_ ->
                let
                    queryResult : Result Query.Error Query.Node
                    queryResult =
                        Query.fromJson triangleWithoutIndices (Query.treeFromNode (Node.Index 0))
                            |> Result.map Tree.label

                    expected : List TriangularMesh
                    expected =
                        [ TriangularMesh Nothing
                            [ ( { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 0 1 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              , { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 1 0 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              , { joints = Nothing
                                , normal = Nothing
                                , tangent = Nothing
                                , color = Nothing
                                , position = vec3 0 0 0
                                , weights = Nothing
                                , texCoords = Nothing
                                }
                              )
                            ]
                        ]
                in
                case queryResult of
                    Ok thing ->
                        Expect.all
                            [ \x ->
                                Expect.equal expected (Query.meshesFromNode x)
                            ]
                            thing

                    Err _ ->
                        Expect.fail "Extract failed"
        , test "Extracts thing with skin" <|
            \_ ->
                let
                    maybeSkin : Maybe Skin
                    maybeSkin =
                        Query.fromJson simpleSkin (Query.treeFromNode (Node.Index 0))
                            |> Result.map (Tree.label >> Query.skinFromNode)
                            |> Result.withDefault Nothing

                    expected : Skin
                    expected =
                        Skin
                            { inverseBindMatrices =
                                [ Mat4.identity
                                , Mat4.makeTranslate (Vec3.vec3 0 -1 0)
                                ]
                            , joints = [ Node.Index 1, Node.Index 2 ]
                            }
                in
                case maybeSkin of
                    Just skin ->
                        Expect.equal
                            expected
                            skin

                    Nothing ->
                        Expect.fail "Extract failed"
        ]


simpleSkin : String
simpleSkin =
    -- https://github.com/KhronosGroup/glTF-Sample-Assets/tree/main/Models/SimpleSkin
    """
{
    "scene" : 0,
    "scenes" : [ {
        "nodes" : [ 0, 1 ]
    } ],

    "nodes" : [ {
        "skin" : 0,
        "mesh" : 0
    }, {
        "children" : [ 2 ]
    }, {
        "translation" : [ 0.0, 1.0, 0.0 ],
        "rotation" : [ 0.0, 0.0, 0.0, 1.0 ]
    } ],

    "meshes" : [ {
        "primitives" : [ {
            "attributes" : {
                "POSITION" : 1,
                "JOINTS_0" : 2,
                "WEIGHTS_0" : 3
            },
            "indices" : 0
        } ]
    } ],

    "skins" : [ {
        "inverseBindMatrices" : 4,
        "joints" : [ 1, 2 ]
    } ],

    "animations" : [ {
        "channels" : [ {
            "sampler" : 0,
            "target" : {
                "node" : 2,
                "path" : "rotation"
            }
        } ],
        "samplers" : [ {
            "input" : 5,
            "interpolation" : "LINEAR",
            "output" : 6
        } ]
    } ],

    "buffers" : [ {
        "uri" : "data:application/gltf-buffer;base64,AAABAAMAAAADAAIAAgADAAUAAgAFAAQABAAFAAcABAAHAAYABgAHAAkABgAJAAgAAAAAvwAAAAAAAAAAAAAAPwAAAAAAAAAAAAAAvwAAAD8AAAAAAAAAPwAAAD8AAAAAAAAAvwAAgD8AAAAAAAAAPwAAgD8AAAAAAAAAvwAAwD8AAAAAAAAAPwAAwD8AAAAAAAAAvwAAAEAAAAAAAAAAPwAAAEAAAAAA",
        "byteLength" : 168
    }, {
        "uri" : "data:application/gltf-buffer;base64,AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAABAPwAAgD4AAAAAAAAAAAAAQD8AAIA+AAAAAAAAAAAAAAA/AAAAPwAAAAAAAAAAAAAAPwAAAD8AAAAAAAAAAAAAgD4AAEA/AAAAAAAAAAAAAIA+AABAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAA=",
        "byteLength" : 320
    }, {
        "uri" : "data:application/gltf-buffer;base64,AACAPwAAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAAAAAACAPwAAgD8AAAAAAAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAIC/AAAAAAAAgD8=",
        "byteLength" : 128
    }, {
        "uri" : "data:application/gltf-buffer;base64,AAAAAAAAAD8AAIA/AADAPwAAAEAAACBAAABAQAAAYEAAAIBAAACQQAAAoEAAALBAAAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAkxjEPkSLbD8AAAAAAAAAAPT9ND/0/TQ/AAAAAAAAAAD0/TQ/9P00PwAAAAAAAAAAkxjEPkSLbD8AAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAkxjEvkSLbD8AAAAAAAAAAPT9NL/0/TQ/AAAAAAAAAAD0/TS/9P00PwAAAAAAAAAAkxjEvkSLbD8AAAAAAAAAAAAAAAAAAIA/",
        "byteLength" : 240
    } ],

    "bufferViews" : [ {
        "buffer" : 0,
        "byteLength" : 48,
        "target" : 34963
    }, {
        "buffer" : 0,
        "byteOffset" : 48,
        "byteLength" : 120,
        "target" : 34962
    }, {
        "buffer" : 1,
        "byteLength" : 320,
        "byteStride" : 16
    }, {
        "buffer" : 2,
        "byteLength" : 128
    }, {
        "buffer" : 3,
        "byteLength" : 240
    } ],

    "accessors" : [ {
        "bufferView" : 0,
        "componentType" : 5123,
        "count" : 24,
        "type" : "SCALAR"
    }, {
        "bufferView" : 1,
        "componentType" : 5126,
        "count" : 10,
        "type" : "VEC3",
        "max" : [ 0.5, 2.0, 0.0 ],
        "min" : [ -0.5, 0.0, 0.0 ]
    }, {
        "bufferView" : 2,
        "componentType" : 5123,
        "count" : 10,
        "type" : "VEC4"
    }, {
        "bufferView" : 2,
        "byteOffset" : 160,
        "componentType" : 5126,
        "count" : 10,
        "type" : "VEC4"
    }, {
        "bufferView" : 3,
        "componentType" : 5126,
        "count" : 2,
        "type" : "MAT4"
    }, {
        "bufferView" : 4,
        "componentType" : 5126,
        "count" : 12,
        "type" : "SCALAR",
        "max" : [ 5.5 ],
        "min" : [ 0.0 ]
    }, {
        "bufferView" : 4,
        "byteOffset" : 48,
        "componentType" : 5126,
        "count" : 12,
        "type" : "VEC4",
        "max" : [ 0.0, 0.0, 0.707, 1.0 ],
        "min" : [ 0.0, 0.0, -0.707, 0.707 ]
    } ],

    "asset" : {
        "version" : "2.0"
    }
}
"""


triangle : String
triangle =
    -- https://github.com/KhronosGroup/glTF-Sample-Assets/tree/main/Models/TriangleWithoutIndices
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


triangleWithoutIndices : String
triangleWithoutIndices =
    -- https://github.com/KhronosGroup/glTF-Sample-Assets/blob/main/Models/TriangleWithoutIndices/README.md
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
      "mesh" : 0
    }
  ],

  "meshes" : [
    {
      "primitives" : [ {
        "attributes" : {
          "POSITION" : 0
        }
      } ]
    }
  ],

  "buffers" : [
    {
      "uri" : "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA",
      "byteLength" : 36
    }
  ],
  "bufferViews" : [
    {
      "buffer" : 0,
      "byteOffset" : 0,
      "byteLength" : 36,
      "target" : 34962
    }
  ],
  "accessors" : [
    {
      "bufferView" : 0,
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
