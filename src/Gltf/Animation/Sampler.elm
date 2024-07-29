module Gltf.Animation.Sampler exposing (Interpolation(..), Sampler(..))

import Gltf.Query.Attribute as Attribute


type alias Attribute =
    Attribute.Attribute


type Sampler
    = Sampler
        { input : List Attribute
        , output : List Attribute
        , interpolation : Interpolation
        }


type Interpolation
    = Linear
    | Step
    | CubicSpline
