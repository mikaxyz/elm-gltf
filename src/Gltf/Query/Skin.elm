module Gltf.Query.Skin exposing (Index(..), Skin(..))

{-| TODO: Docs

@docs Index, Skin

-}

import Gltf.Query.NodeIndex exposing (NodeIndex)
import Gltf.Query.Skeleton exposing (Skeleton)
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
