module Model exposing
    ( Modal(..)
    , Model
    , Msg(..)
    , init
    )

import Http
import Page exposing (Page)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import SampleAssets exposing (SampleAssets, SampleType)
import Xyz.Mika.Spa as Spa exposing (Spa)


type Msg
    = OnRouteChange (Maybe Route)
    | OnHistoryChange Route
    | SpaMsg Spa.Msg
    | PageMsg Page.Msg
    | UserClickedSampleType SampleType
    | SampleAssetsReceived (Result Http.Error SampleAssets)
    | ShowModal (Maybe Modal)
    | ShowShareSheet
    | ToggleSampleAssets


type alias Model =
    { spa : Spa Route
    , page : Maybe Page
    , modal : Maybe Modal
    , sampleAssets : RemoteData Http.Error SampleAssets
    , sampleType : SampleType
    , sampleAssetsVisible : Bool
    }


type Modal
    = Help


init : Spa Route -> Model
init spa =
    { spa = spa
    , page = Nothing
    , modal = Nothing
    , sampleAssets = RemoteData.Loading
    , sampleType = SampleAssets.Default
    , sampleAssetsVisible = False
    }
