module Internal.Skin exposing
    ( Index(..)
    , JointNodeIndex(..)
    , Skin
    , decoder
    , indexDecoder
    )

import Internal.Accessor as Accessor
import Json.Decode as JD


type Index
    = Index Int


type JointNodeIndex
    = JointNodeIndex Int


type alias Skin =
    { inverseBindMatrices : Maybe Accessor.Index
    , joints : List JointNodeIndex
    }


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Skin
decoder =
    JD.map2 Skin
        (JD.maybe (JD.field "inverseBindMatrices" Accessor.indexDecoder))
        (JD.field "joints" (JD.list (JD.int |> JD.map JointNodeIndex)))
