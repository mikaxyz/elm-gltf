module Gltf.Animation.Animation exposing (Animation(..))

import Array exposing (Array)
import Gltf.Animation.Channel exposing (Channel)
import Gltf.Animation.Sampler exposing (Sampler)


type Animation
    = Animation
        { name : Maybe String
        , samplers : Array Sampler
        , channels : List Channel
        }
