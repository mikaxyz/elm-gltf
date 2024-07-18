module Gltf.Query.Skeleton exposing (Bone, Skeleton(..))

{-| TODO: Docs

@docs Bone, Skeleton

-}

import Gltf.Query.NodeIndex exposing (NodeIndex)
import Gltf.Query.Transform exposing (Transform)
import Tree exposing (Tree)


{-| TODO: Docs
-}
type alias Bone =
    { nodeIndex : NodeIndex
    , transform : Transform
    }


{-| TODO: Docs
-}
type Skeleton
    = Skeleton (Tree Bone)
