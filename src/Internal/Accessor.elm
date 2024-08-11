module Internal.Accessor exposing
    ( Accessor
    , ComponentType(..)
    , Index(..)
    , Type(..)
    , decoder
    , indexDecoder
    )

import Internal.BufferView as BufferView
import Internal.Util as Util
import Json.Decode as JD


type Index
    = Index Int


type alias Accessor =
    { componentType : ComponentType
    , type_ : Type
    , count : Int
    , min : List Float
    , max : List Float
    , bufferView : BufferView.Index
    , byteOffset : Int
    }


type ComponentType
    = BYTE
    | UNSIGNED_BYTE
    | SHORT
    | UNSIGNED_SHORT
    | UNSIGNED_INT
    | FLOAT


type Type
    = SCALAR
    | VEC2
    | VEC3
    | VEC4
    | MAT2
    | MAT3
    | MAT4


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Accessor
decoder =
    JD.map7 Accessor
        (JD.field "componentType" componentTypeDecoder)
        (JD.field "type" typeDecoder)
        (JD.field "count" JD.int)
        (Util.optionalField "min" (JD.list JD.float) [])
        (Util.optionalField "max" (JD.list JD.float) [])
        (JD.field "bufferView" BufferView.indexDecoder)
        (Util.optionalField "byteOffset" JD.int 0)


componentTypeDecoder : JD.Decoder ComponentType
componentTypeDecoder =
    JD.int
        |> JD.andThen
            (\value ->
                case value of
                    5120 ->
                        JD.succeed BYTE

                    5121 ->
                        JD.succeed UNSIGNED_BYTE

                    5122 ->
                        JD.succeed SHORT

                    5123 ->
                        JD.succeed UNSIGNED_SHORT

                    5125 ->
                        JD.succeed UNSIGNED_INT

                    5126 ->
                        JD.succeed FLOAT

                    _ ->
                        JD.fail <| "Could not decode ComponentType from " ++ String.fromInt value
            )


typeDecoder : JD.Decoder Type
typeDecoder =
    JD.string
        |> JD.andThen
            (\value ->
                case value of
                    "SCALAR" ->
                        JD.succeed SCALAR

                    "VEC2" ->
                        JD.succeed VEC2

                    "VEC3" ->
                        JD.succeed VEC3

                    "VEC4" ->
                        JD.succeed VEC4

                    "MAT2" ->
                        JD.succeed MAT2

                    "MAT3" ->
                        JD.succeed MAT3

                    "MAT4" ->
                        JD.succeed MAT4

                    _ ->
                        JD.fail <| "Could not decode Type from " ++ value
            )
