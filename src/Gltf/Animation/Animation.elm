module Gltf.Animation.Animation exposing (Animation(..))

import Gltf.Animation.Channel exposing (Channel)


type Animation
    = Animation
        { name : Maybe String
        , channels : List Channel
        }
