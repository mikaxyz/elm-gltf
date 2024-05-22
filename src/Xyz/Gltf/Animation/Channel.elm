module Xyz.Gltf.Animation.Channel exposing
    ( Channel(..)
    , Index(..)
    , Path(..)
    , decoder
    , indexDecoder
    )

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Xyz.Gltf.Animation.Sampler as Sampler
import Xyz.Gltf.Node as Node


type Index
    = Index Int


type Channel
    = Channel Data


type alias Data =
    { sampler : Sampler.Index
    , target : Target
    }


type alias Target =
    { node : Node.Index
    , path : Path
    }


type Path
    = Translation
    | Rotation
    | Scale
    | Weights


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Channel
decoder =
    JD.succeed Data
        |> JDP.required "sampler" Sampler.indexDecoder
        |> JDP.required "target" targetDecoder
        |> JD.map Channel


targetDecoder : JD.Decoder Target
targetDecoder =
    JD.succeed Target
        |> JDP.required "node" Node.indexDecoder
        |> JDP.required "path" pathDecoder


pathDecoder : JD.Decoder Path
pathDecoder =
    JD.string
        |> JD.andThen
            (\id ->
                case id of
                    "translation" ->
                        JD.succeed Translation

                    "rotation" ->
                        JD.succeed Rotation

                    "scale" ->
                        JD.succeed Scale

                    "weights" ->
                        JD.succeed Weights

                    _ ->
                        JD.fail ("unknown value for Path: " ++ id)
            )
