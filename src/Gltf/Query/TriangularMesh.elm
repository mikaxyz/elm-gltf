module Gltf.Query.TriangularMesh exposing
    ( Material(..)
    , TriangularMesh(..)
    , Vertex
    , fromPrimitive
    )

import Common
import Gltf exposing (Gltf)
import Gltf.Query.Attribute as Attribute exposing (Attribute)
import Gltf.Query.Material
import Gltf.Query.ResolvedMaterial
import Gltf.Query.VertexBuffers as VertexBuffers exposing (VertexBuffers)
import Internal.Accessor as Accessor
import Internal.Mesh exposing (Primitive)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


type alias Vertex =
    { position : Vec3
    , normal : Maybe Vec3
    , weights : Maybe Vec4
    , joints : Maybe Joints
    , texCoords : Maybe Vec2
    }


type alias Joints =
    { j1 : Int
    , j2 : Int
    , j3 : Int
    , j4 : Int
    }


type TriangularMesh
    = TriangularMesh (Maybe Material) (List ( Vertex, Vertex, Vertex ))
    | IndexedTriangularMesh (Maybe Material) ( List Vertex, List ( Int, Int, Int ) )


type Material
    = Material Gltf.Query.Material.Material
    | ResolvedMaterial Gltf.Query.ResolvedMaterial.Material


type alias VertexAttributes =
    { position : Maybe (List Attribute)
    , normal : Maybe (List Attribute)
    , joints : Maybe (List Attribute)
    , weights : Maybe (List Attribute)
    , texCoords : Maybe (List Attribute)
    }


fromPrimitive : Gltf -> Primitive -> TriangularMesh
fromPrimitive gltf primitive =
    let
        vertexBuffers : VertexBuffers
        vertexBuffers =
            VertexBuffers.fromPrimitive gltf primitive

        vertexAttributes : VertexAttributes
        vertexAttributes =
            { position = Maybe.map Attribute.parseBuffer vertexBuffers.position
            , normal = Maybe.map Attribute.parseBuffer vertexBuffers.normal
            , joints = Maybe.map Attribute.parseBuffer vertexBuffers.joints
            , weights = Maybe.map Attribute.parseBuffer vertexBuffers.weights
            , texCoords = Maybe.map Attribute.parseBuffer vertexBuffers.texCoords
            }

        attributeToJoints : Attribute -> Maybe { j1 : Int, j2 : Int, j3 : Int, j4 : Int }
        attributeToJoints a =
            case a of
                Attribute.Vec4IntAttribute { x, y, z, w } ->
                    Just
                        { j1 = x
                        , j2 = y
                        , j3 = z
                        , j4 = w
                        }

                _ ->
                    Nothing

        vertexAttributesToVertices2 : VertexAttributes -> List Vertex
        vertexAttributesToVertices2 a =
            let
                positions : List Vertex
                positions =
                    a.position
                        |> Maybe.withDefault []
                        |> List.filterMap Attribute.toVec3
                        |> List.map
                            (\position ->
                                { position = position
                                , normal = Nothing
                                , weights = Nothing
                                , joints = Nothing
                                , texCoords = Nothing
                                }
                            )

                withNormals : List Vertex -> List Vertex
                withNormals vertices =
                    case a.normal |> Maybe.withDefault [] |> List.filterMap Attribute.toVec3 of
                        [] ->
                            vertices

                        normals ->
                            List.map2 (\vertex x -> { vertex | normal = Just x })
                                vertices
                                normals

                withWeights : List Vertex -> List Vertex
                withWeights vertices =
                    case a.weights |> Maybe.withDefault [] |> List.filterMap Attribute.toVec4 of
                        [] ->
                            vertices

                        weights ->
                            List.map2 (\vertex x -> { vertex | weights = Just x })
                                vertices
                                weights

                withJoints : List Vertex -> List Vertex
                withJoints vertices =
                    case a.joints |> Maybe.withDefault [] |> List.filterMap attributeToJoints of
                        [] ->
                            vertices

                        weights ->
                            List.map2 (\vertex x -> { vertex | joints = Just x })
                                vertices
                                weights

                withTexCoords : List Vertex -> List Vertex
                withTexCoords vertices =
                    case a.texCoords |> Maybe.withDefault [] |> List.filterMap Attribute.toVec2 of
                        [] ->
                            vertices

                        texCoords ->
                            List.map2 (\vertex x -> { vertex | texCoords = Just x })
                                vertices
                                texCoords
            in
            positions
                |> withNormals
                |> withWeights
                |> withJoints
                |> withTexCoords

        material : Maybe Material
        material =
            Gltf.Query.Material.fromPrimitive gltf primitive |> Maybe.map Material
    in
    case primitive.indices of
        Just indices ->
            IndexedTriangularMesh material
                ( vertexAttributesToVertices2 vertexAttributes
                , readTriangleIndices gltf indices
                )

        Nothing ->
            vertexAttributes
                |> vertexAttributesToVertices2
                |> List.reverse
                |> List.foldl
                    (\vertex ( c, a ) ->
                        case c of
                            [ v2, v1 ] ->
                                ( [], ( v1, v2, vertex ) :: a )

                            _ ->
                                ( vertex :: c, a )
                    )
                    ( [], [] )
                |> Tuple.second
                |> TriangularMesh material


readTriangleIndices : Gltf -> Accessor.Index -> List ( Int, Int, Int )
readTriangleIndices gltf indices =
    Common.accessorAtIndex gltf indices
        |> Maybe.andThen (Common.bufferInfo gltf)
        |> Maybe.map Attribute.parseBuffer
        |> Maybe.withDefault []
        |> List.filterMap
            (\a ->
                case a of
                    Attribute.ScalarIntAttribute v ->
                        Just v

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\val ( curr, acc ) ->
                case curr of
                    [ y, x ] ->
                        ( [], ( x, y, val ) :: acc )

                    _ ->
                        ( val :: curr, acc )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse
