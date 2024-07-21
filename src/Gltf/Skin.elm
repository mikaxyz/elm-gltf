module Gltf.Skin exposing (Index(..), Skin(..), Skeleton)

{-| TODO: Docs

@docs Index, Skin, Skeleton

-}

import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Query.Skeleton
import Math.Matrix4 exposing (Mat4)


{-| TODO: Docs
-}
type Index
    = Index Int


{-| TODO: Docs
-}
type Skin
    = Skin
        { inverseBindMatrices : List Mat4
        , joints : List NodeIndex
        , index : Index
        , skeleton : Skeleton
        }


{-| TODO: Docs
-}
type alias Skeleton =
    Gltf.Query.Skeleton.Skeleton
