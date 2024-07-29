module Internal.BufferView exposing
    ( BufferView
    , Index(..)
    , decoder
    , indexDecoder
    )

import Internal.Buffer as Buffer
import Internal.Util as Util
import Json.Decode as JD


type Index
    = Index Int


type alias BufferView =
    { buffer : Buffer.Index
    , byteLength : Int
    , byteOffset : Int
    , byteStride : Int
    , target : Target
    }


type Target
    = ArrayBuffer
    | ElementArrayBuffer


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder BufferView
decoder =
    JD.map5 BufferView
        (JD.field "buffer" Buffer.indexDecoder)
        (JD.field "byteLength" JD.int)
        (Util.optionalField "byteOffset" JD.int 0)
        (Util.optionalField "byteStride" JD.int 0)
        (Util.optionalField "target" targetDecoder ArrayBuffer)


targetDecoder : JD.Decoder Target
targetDecoder =
    JD.int
        |> JD.andThen
            (\value ->
                case value of
                    34962 ->
                        JD.succeed ArrayBuffer

                    34963 ->
                        JD.succeed ElementArrayBuffer

                    _ ->
                        JD.fail <| "Could not decode Target from " ++ String.fromInt value
            )
