module Internal.Scene exposing
    ( Index(..)
    , Scene(..)
    , decoder
    )

import Internal.Node as Node
import Internal.Util as Util
import Json.Decode as JD


type Index
    = Index Int


type Scene
    = Scene Data


type alias Data =
    { name : Maybe String
    , nodes : List Node.Index
    }


decoder : JD.Decoder Scene
decoder =
    JD.map2 Data
        (Util.optionalField "name" (JD.maybe JD.string) Nothing)
        (JD.field "nodes" (JD.list Node.indexDecoder))
        |> JD.map Scene
