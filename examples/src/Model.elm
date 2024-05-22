module Model exposing
    ( Model
    , Msg(..)
    , init
    )

import Http
import Page exposing (Page)
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import SampleAssets exposing (SampleAssets)
import Xyz.Mika.Spa as Spa exposing (Spa)


type Msg
    = OnRouteChange (Maybe Route)
    | OnHistoryChange Route
    | SpaMsg Spa.Msg
    | PageMsg Page.Msg
    | SampleAssetsReceived (Result Http.Error SampleAssets)


type alias Model =
    { spa : Spa Route
    , page : Maybe Page
    , sampleAssets : RemoteData Http.Error SampleAssets
    }


init : Spa Route -> Model
init spa =
    { spa = spa
    , page = Nothing
    , sampleAssets = RemoteData.Loading
    }
