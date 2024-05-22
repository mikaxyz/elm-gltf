module Xyz.Gltf.Animation exposing
    ( Animation(..)
    , Index(..)
    , decoder
    , indexDecoder
    )

import Array exposing (Array)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Xyz.Gltf.Animation.Channel as Channel exposing (Channel)
import Xyz.Gltf.Animation.Sampler as Sampler exposing (Sampler)


type Index
    = Index Int


type Animation
    = Animation Data


type alias Data =
    { name : Maybe String
    , channels : Array Channel
    , samplers : Array Sampler
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Animation
decoder =
    JD.succeed Data
        |> JDP.optional "name" (JD.maybe JD.string) Nothing
        |> JDP.required "channels" (JD.array Channel.decoder)
        |> JDP.required "samplers" (JD.array Sampler.decoder)
        |> JD.map Animation
