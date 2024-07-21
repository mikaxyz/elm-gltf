module Gltf.Query.TriangularMesh exposing (TriangularMesh(..), Joints, Vertex)

{-| TODO: Docs

@docs TriangularMesh, Joints, Vertex

-}

import Gltf.Material exposing (Material)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| TODO: Docs
-}
type TriangularMesh
    = TriangularMesh (Maybe Material) (List ( Vertex, Vertex, Vertex ))
    | IndexedTriangularMesh (Maybe Material) ( List Vertex, List ( Int, Int, Int ) )


{-| TODO: Docs
-}
type alias Vertex =
    { position : Vec3
    , normal : Maybe Vec3
    , tangent : Maybe Vec4
    , color : Maybe Vec4
    , weights : Maybe Vec4
    , joints : Maybe Joints
    , texCoords : Maybe Vec2
    }


{-| TODO: Docs
-}
type alias Joints =
    { j1 : Int
    , j2 : Int
    , j3 : Int
    , j4 : Int
    }
