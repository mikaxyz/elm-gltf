module Gltf.Query.Skeleton exposing (Bone, Skeleton(..))

import Math.Matrix4 exposing (Mat4)
import Tree exposing (Tree)


{-| A bone with all time-independent data resolved: its node index, skin index,
inverse bind matrix, and the static translation/rotation/scale matrices used when
a TRS component is not animated. Built once when a Skin is loaded so per-frame
bone animation does no lookup-table or matrix construction work.
-}
type alias Bone =
    { nodeIndex : Int
    , skinIndex : Int
    , inverseBindMatrix : Mat4
    , staticTranslation : Mat4
    , staticRotation : Mat4
    , staticScale : Mat4
    }


{-| The skeleton's base transform plus a tree of [Bones](#Bone).
-}
type Skeleton
    = Skeleton Mat4 (Tree Bone)
