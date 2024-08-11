module Gltf.Skin exposing (Index(..), Skin(..), Skeleton)

{-| The [Meshes](Gltf-Mesh#Mesh) of a [SkinnedMeshNode](Gltf-Node#Node) can be deformed/animated using a Skin.

@docs Index, Skin, Skeleton

-}

import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Query.Skeleton
import Math.Matrix4 exposing (Mat4)


{-| The identifier for a Skin
-}
type Index
    = Index Int


{-| Joints and matrices defining a skin.
-}
type Skin
    = Skin
        { inverseBindMatrices : List Mat4
        , joints : List NodeIndex
        , index : Index
        , skeleton : Skeleton
        }


{-| Internal
-}
type alias Skeleton =
    Gltf.Query.Skeleton.Skeleton
