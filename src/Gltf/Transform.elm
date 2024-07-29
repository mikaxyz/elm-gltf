module Gltf.Transform exposing (Transform(..))

{-| The [Nodes](Gltf-Node#Node) in glTF define their local [transformations](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#transformations) as either as a [matrix](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/Math-Matrix4#Mat4) or a combination of [translation](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/Math-Vector3#Vec3), [rotation](https://package.elm-lang.org/packages/nphollon/geo3d/latest/Quaternion#Quaternion), and [scale](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/Math-Vector3#Vec3) (also known as TRS properties).

@docs Transform

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


{-| The local transform of a [Node](Gltf-Node#Node).
-}
type Transform
    = TRS
        { translation : Maybe Vec3
        , rotation : Maybe Quaternion
        , scale : Maybe Vec3
        }
    | Matrix Mat4
