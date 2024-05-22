module Xyz.Gltf.Raw.Glb exposing (decoder)

{-| Decoder for binary (.glb) files

@docs decoder

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Parser as Parser
import Json.Decode
import Xyz.Gltf.Buffer
import Xyz.Gltf.Raw.Gltf as Gltf exposing (Gltf)


type alias Header =
    { magic : Int
    , version : Int
    , length : Int
    }


type alias Chunk =
    { length : Int, type_ : ChunkType, data : Bytes }


type ChunkType
    = Json
    | Binary


{-| Decoder
-}
decoder : Decode.Decoder Gltf
decoder =
    Decode.map3 (\header json buffers -> { header = header, chunk1 = json, chunk2 = buffers })
        headerDecoder
        chunkDecoder
        chunkDecoder
        |> Decode.andThen
            (\{ header, chunk1, chunk2 } ->
                case chunk1.type_ of
                    Json ->
                        case parseString chunk1.length chunk1.data of
                            Ok json ->
                                let
                                    jsonDecoder =
                                        Gltf.decoderWithSingleBuffer (Xyz.Gltf.Buffer.Buffer chunk2.data)
                                in
                                case Json.Decode.decodeString jsonDecoder json of
                                    Ok gltf ->
                                        Decode.succeed gltf

                                    Err _ ->
                                        Decode.fail

                            Err _ ->
                                Decode.fail

                    Binary ->
                        Decode.fail
            )


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
