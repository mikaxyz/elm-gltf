module Xyz.Gltf.Raw.Glb exposing (decoder)

{-| Decoder for binary (.glb) files

@docs decoder

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Parser as Parser
import Xyz.Gltf.Buffer


type alias Chunk =
    { length : Int, type_ : ChunkType, data : Bytes }


type alias Header =
    { magic : Int
    , version : Int
    , length : Int
    }


type ChunkType
    = Json
    | Binary


type alias Glb =
    { header : Header
    , jsonString : String
    , buffers : Xyz.Gltf.Buffer.Buffer
    }


{-| Bytes Decoder
-}
decoder : Decode.Decoder Glb
decoder =
    Decode.map3 Glb
        headerDecoder
        (chunkDecoder
            |> Decode.andThen
                (\chunk ->
                    case chunk.type_ of
                        Json ->
                            case parseString chunk.length chunk.data of
                                Ok jsonString ->
                                    Decode.succeed jsonString

                                Err _ ->
                                    Decode.fail

                        Binary ->
                            Decode.fail
                )
        )
        (chunkDecoder |> Decode.map (.data >> Xyz.Gltf.Buffer.Buffer))


headerDecoder : Decode.Decoder Header
headerDecoder =
    Decode.map3 Header
        magicDecoder
        (Decode.unsignedInt32 Bytes.LE)
        (Decode.unsignedInt32 Bytes.LE)


magicDecoder : Decode.Decoder Int
magicDecoder =
    Decode.unsignedInt32 Bytes.LE
        |> Decode.andThen
            (\v ->
                if v == 0x46546C67 then
                    Decode.succeed v

                else
                    Decode.fail
            )


chunkDecoder : Decode.Decoder Chunk
chunkDecoder =
    Decode.map2
        Tuple.pair
        (Decode.unsignedInt32 Bytes.LE)
        (Decode.unsignedInt32 Bytes.LE)
        |> Decode.andThen
            (\( chunkLength, chunkType ) ->
                Decode.bytes chunkLength
                    |> Decode.andThen
                        (\data ->
                            case chunkType of
                                0x4E4F534A ->
                                    { length = chunkLength
                                    , type_ = Json
                                    , data = data
                                    }
                                        |> Decode.succeed

                                0x004E4942 ->
                                    { length = chunkLength
                                    , type_ = Binary
                                    , data = data
                                    }
                                        |> Decode.succeed

                                _ ->
                                    Decode.fail
                        )
            )


parseString : Int -> Bytes -> Result (Parser.Error context error) String
parseString length =
    Parser.run (Parser.string length)
