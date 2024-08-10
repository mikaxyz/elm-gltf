module Gltf.Query.Skeleton exposing (Bone, Skeleton(..))

import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Transform exposing (Transform)
import Math.Matrix4 exposing (Mat4)
import Tree exposing (Tree)


type alias Bone =
    { nodeIndex : NodeIndex
    , transform : Transform
    }


type Skeleton
    = Skeleton Mat4 (Tree Bone)
