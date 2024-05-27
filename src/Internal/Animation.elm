module Internal.Animation exposing
    ( Animation(..)
    , Index(..)
    , decoder
    , indexDecoder
    )

import Array exposing (Array)
import Internal.Animation.Channel as Channel exposing (Channel)
import Internal.Animation.Sampler as Sampler exposing (Sampler)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


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
