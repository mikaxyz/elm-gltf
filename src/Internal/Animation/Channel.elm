module Internal.Animation.Channel exposing
    ( Channel(..)
    , Path(..)
    , decoder
    )

import Internal.Animation.Sampler as Sampler
import Internal.Node as Node
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


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
