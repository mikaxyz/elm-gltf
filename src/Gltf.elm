module Gltf exposing
    ( getBinary
    , getEmbedded
    )

{-| Import 3d assets from glTF (Graphics Library Transmission Format) file format

@docs getBinary
@docs getEmbedded

-}

import Http
import Xyz.Gltf.Raw.Glb as Glb
import Xyz.Gltf.Raw.Gltf as Gltf exposing (Gltf)


{-| Get contents of a file of type .glb
-}
getBinary : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getBinary url msg =
    Http.get
        { url = url
        , expect = Http.expectBytes msg Glb.decoder
        }


{-| Get contents of a file of type .gltf
-}
getEmbedded : String -> (Result Http.Error Gltf -> msg) -> Cmd msg
getEmbedded url msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg Gltf.decoder
        }
