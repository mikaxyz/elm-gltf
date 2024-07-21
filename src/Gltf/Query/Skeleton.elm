module Gltf.Query.Skeleton exposing (Bone, Skeleton(..))

import Gltf.Query.NodeIndex exposing (NodeIndex)
import Gltf.Transform exposing (Transform)
import Tree exposing (Tree)


type alias Bone =
    { nodeIndex : NodeIndex
    , transform : Transform
    }


type Skeleton
    = Skeleton (Tree Bone)
