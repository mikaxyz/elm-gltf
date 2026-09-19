module Gltf.Animation.Animation exposing (Animation(..))

import Dict exposing (Dict)
import Gltf.Animation.Channel exposing (Channel)


type Animation
    = Animation
        { name : Maybe String
        , startTime : Float
        , endTime : Float
        , channels : List Channel

        -- Channels keyed by ( node index, path ) for fast per-frame lookup during
        -- skinned animation. Precomputed once when the animation is loaded.
        , channelsByNode : Dict ( Int, String ) Channel
        }
