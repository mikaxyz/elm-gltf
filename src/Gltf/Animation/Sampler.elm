module Gltf.Animation.Sampler exposing (Interpolation(..), Keyframes(..), Sampler(..))

import Array exposing (Array)
import Math.Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type Sampler
    = Sampler
        { inputMin : Float
        , inputMax : Float
        , interpolation : Interpolation
        , keyframes : Keyframes
        }


type Keyframes
    = Vec3Keyframes (Array ( Float, Vec3 ))
    | QuaternionKeyframes (Array ( Float, Quaternion ))


type Interpolation
    = Linear
    | Step
    | CubicSpline
