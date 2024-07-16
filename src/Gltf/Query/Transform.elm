module Gltf.Query.Transform exposing (Transform(..))

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type Transform
    = TRS
        { rotation : Maybe Quaternion
        , translation : Maybe Vec3
        , scale : Maybe Vec3
        }
    | Matrix Mat4
