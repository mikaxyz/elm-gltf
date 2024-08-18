module Gltf.Mesh exposing (Mesh(..), Vertex, Joints)

{-| A mesh contains the geometry (vertices) and the material of a 3D object. One [Node](Gltf-Node#Node) can hold a multiple of meshes.

**NOTE:** This package supplies meshes with a "concrete" type of attribute named [Vertex](Gltf-Mesh#Vertex). The thinking here is that your application will map these into your own type depending of needs. Storing meshes with this [Vertex](Gltf-Mesh#Vertex) type will mean storing a lot of redundant data and is not recommended if goal is optimization.

@docs Mesh, Vertex, Joints

-}

import Gltf.Material exposing (Material)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| The types of meshes as you will find in [elm-explorations/webgl](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL#meshes)
-}
type Mesh
    = Points (Maybe Material) (List Vertex)
    | Lines (Maybe Material) (List ( Vertex, Vertex ))
    | LineLoop (Maybe Material) (List Vertex)
    | LineStrip (Maybe Material) (List Vertex)
    | TriangleStrip (Maybe Material) (List Vertex)
    | TriangleFan (Maybe Material) (List Vertex)
    | TriangularMesh (Maybe Material) (List ( Vertex, Vertex, Vertex ))
    | IndexedTriangularMesh (Maybe Material) ( List Vertex, List ( Int, Int, Int ) )


{-| The "attributes" of the Mesh. Map these into the attribute your shader(s) use.
-}
type alias Vertex =
    { position : Vec3
    , normal : Maybe Vec3
    , tangent : Maybe Vec4
    , color : Maybe Vec4
    , weights : Maybe Vec4
    , joints : Maybe Joints
    , texCoord0 : Maybe Vec2
    , texCoord1 : Maybe Vec2
    }


{-| The indices of the joints in the [skeleton](Gltf-Skin#Skeleton) the vertex is effected by.
-}
type alias Joints =
    { j1 : Int
    , j2 : Int
    , j3 : Int
    , j4 : Int
    }
