module Gltf.Transform exposing (Transform(..))

{-| TODO: Docs

@docs Transform

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


{-| TODO: Docs
-}
type Transform
    = TRS
        { rotation : Maybe Quaternion
        , translation : Maybe Vec3
        , scale : Maybe Vec3
        }
    | Matrix Mat4
