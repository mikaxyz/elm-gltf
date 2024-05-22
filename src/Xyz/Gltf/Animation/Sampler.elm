module Xyz.Gltf.Animation.Sampler exposing
    ( Index(..)
    , Interpolation(..)
    , Sampler(..)
    , decoder
    , indexDecoder
    )

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Xyz.Gltf.Accessor as Accessor


type Index
    = Index Int


type Sampler
    = Sampler Data


type alias Data =
    { input : Accessor.Index
    , output : Accessor.Index
    , interpolation : Interpolation
    }


type Interpolation
    = Linear
    | Step
    | Cubicspline


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Sampler
decoder =
    JD.succeed Data
        |> JDP.required "input" Accessor.indexDecoder
        |> JDP.required "output" Accessor.indexDecoder
        |> JDP.optional "interpolation" interpolationDecoder Linear
        |> JD.map Sampler


interpolationDecoder : JD.Decoder Interpolation
interpolationDecoder =
    JD.string
        |> JD.andThen
            (\id ->
                case id of
                    "STEP" ->
                        JD.succeed Step

                    "CUBICSPLINE" ->
                        JD.succeed Cubicspline

                    "LINEAR" ->
                        JD.succeed Linear

                    _ ->
                        JD.fail <| "Invalid interpolation type: " ++ id
            )
