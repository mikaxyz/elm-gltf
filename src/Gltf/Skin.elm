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


{-| Internal. The skeleton with all time-independent bone data resolved at load,
sampled per-frame by [Gltf.Animation.animatedBoneTransforms](Gltf-Animation#animatedBoneTransforms).
-}
type alias Skeleton =
    Gltf.Query.Skeleton.Skeleton
