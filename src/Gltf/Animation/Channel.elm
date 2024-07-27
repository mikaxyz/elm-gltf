module Gltf.Animation.Channel exposing (Channel(..), Path(..))

import Gltf.Animation.Sampler exposing (Sampler)
import Gltf.NodeIndex exposing (NodeIndex)


type Channel
    = Channel
        { sampler : Sampler
        , nodeIndex : NodeIndex
        , path : Path
        }


type Path
    = Translation
    | Rotation
    | Scale
    | Weights
