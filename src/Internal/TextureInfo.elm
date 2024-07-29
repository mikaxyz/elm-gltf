module Internal.TextureInfo exposing
    ( Index(..)
    , TextureInfo
    , decoder
    , indexDecoder
    )

import Internal.Texture as Texture
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type Index
    = Index Int


type alias TextureInfo =
    { index : Texture.Index
    , texCoord : Int
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder TextureInfo
decoder =
    JD.succeed TextureInfo
        |> JDP.required "index" Texture.indexDecoder
        |> JDP.optional "texCoord" JD.int 0
