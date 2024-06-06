module Gltf exposing
    ( Gltf, Asset
    , getBinary, getEmbedded
    , decoder, bytesDecoder
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format

@docs Gltf, Asset
@docs getBinary, getEmbedded
@docs decoder, bytesDecoder

-}

import Bytes.Decode
import Http
import Internal.Gltf
import Json.Decode as JD


{-| Type representing raw data from a .gltf/.glb file
-}
type alias Gltf =
    Internal.Gltf.Gltf


{-| Information about the file
-}
type alias Asset =
    { version : String
    , copyright : Maybe String
    , generator : Maybe String
    }


{-| Json Decoder
-}
decoder : JD.Decoder Gltf
decoder =
    Internal.Gltf.decoder


{-| Bytes Decoder
-}
bytesDecoder : Bytes.Decode.Decoder Gltf
bytesDecoder =
    Internal.Gltf.bytesDecoder


{-| Get contents of a file of type .glb
-}
getBinary : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getBinary url msg =
    Http.get
        { url = url
        , expect = Http.expectBytes msg bytesDecoder
        }


{-| Get contents of a file of type .gltf
-}
getEmbedded : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getEmbedded url msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg decoder
        }
