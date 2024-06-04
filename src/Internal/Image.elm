module Internal.Image exposing
    ( Image
    , Index(..)
    , MimeType(..)
    , decoder
    , indexDecoder
    )

import Internal.BufferView as BufferView
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type Index
    = Index Int


type alias Image =
    { name : Maybe String
    , uri : Maybe String
    , mimeType : Maybe MimeType
    , bufferView : Maybe BufferView.Index
    }


type MimeType
    = Jpeg
    | Png


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Image
decoder =
    JD.succeed Image
        |> JDP.optional "name" (JD.maybe JD.string) Nothing
        |> JDP.optional "uri" (JD.maybe JD.string) Nothing
        |> JDP.optional "mimeType" (JD.maybe mimeTypeDecoder) Nothing
        |> JDP.optional "bufferView" (JD.maybe BufferView.indexDecoder) Nothing


mimeTypeDecoder : JD.Decoder MimeType
mimeTypeDecoder =
    JD.string
        |> JD.andThen
            (\mime ->
                case mime of
                    "image/jpeg" ->
                        JD.succeed Jpeg

                    "image/png" ->
                        JD.succeed Png

                    m ->
                        JD.fail <| "Unknown mime type " ++ m
            )
