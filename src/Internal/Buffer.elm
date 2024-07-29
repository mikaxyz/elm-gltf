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
    | Remote { byteLength : Int, uri : String }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Buffer
decoder =
    JD.field "uri" JD.string
        |> JD.andThen
            (\uri ->
                if String.startsWith "data:application" uri then
                    base64decoder

                else
                    JD.map (\byteLength -> Remote { byteLength = byteLength, uri = uri })
                        (JD.field "byteLength" JD.int)
            )


base64decoder : JD.Decoder Buffer
base64decoder =
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
