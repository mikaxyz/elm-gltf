module Internal.Texture exposing
    ( Index(..)
    , Texture
    , decoder
    , indexDecoder
    )

import Internal.Image as Image
import Internal.Sampler as Sampler
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type Index
    = Index Int


type alias Texture =
    { name : Maybe String
    , sampler : Maybe Sampler.Index
    , source : Maybe Image.Index
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Texture
decoder =
    JD.succeed Texture
        |> JDP.optional "name" (JD.maybe JD.string) Nothing
        |> JDP.optional "sampler" (JD.maybe Sampler.indexDecoder) Nothing
        |> JDP.optional "source" (JD.maybe Image.indexDecoder) Nothing
