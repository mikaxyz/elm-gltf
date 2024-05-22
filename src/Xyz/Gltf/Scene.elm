module Xyz.Gltf.Scene exposing
    ( Index(..)
    , Scene(..)
    , decoder
    )

import Json.Decode as JD
import Xyz.Gltf.Node as Node


type Index
    = Index Int


type Scene
    = Scene Data


type alias Data =
    { nodes : List Node.Index
    }


decoder : JD.Decoder Scene
decoder =
    JD.map Data
        (JD.field "nodes" (JD.list Node.indexDecoder))
        |> JD.map Scene
