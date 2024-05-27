module Internal.Buffer exposing
    ( Buffer(..)
    , Index(..)
    , decoder
    , indexDecoder
    )

import Base64
import Bytes exposing (Bytes)
import Json.Decode as JD


type Index
    = Index Int


type Buffer
    = Buffer Bytes


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Buffer
decoder =
    JD.map Buffer
        (JD.field "uri" JD.string
            |> JD.map (String.replace "data:application/gltf-buffer;base64," "")
            |> JD.map (String.replace "data:application/octet-stream;base64," "")
            |> JD.andThen
                (\base64 ->
                    base64
                        |> Base64.toBytes
                        |> Maybe.map JD.succeed
                        |> Maybe.withDefault (JD.fail "Failed to decode buffer")
                )
        )
