module Update exposing
    ( spaConfig
    , update
    )

import Model exposing (Model, Msg(..))
import Page
import Ports
import RemoteData
import Route exposing (Route)
import Xyz.Mika.Spa as Spa


spaConfig : Spa.Config Route Msg
spaConfig =
    { routeFromUrl = Route.fromUrl
    , routeToPath = Route.toPath
    , onRouteChange = OnRouteChange
    , onHistoryChange = OnHistoryChange
    }


updateWithRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
updateWithRoute maybeRoute model =
    Page.fromRoute
        { pushRoute = Spa.pushRoute spaConfig model.spa
        , assets = model.sampleAssets
        }
        PageMsg
        maybeRoute
        |> Tuple.mapFirst (\page -> { model | page = Just page })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRouteChange maybeRoute ->
            updateWithRoute maybeRoute model

        OnHistoryChange _ ->
            ( model, Cmd.none )

        SpaMsg msg_ ->
            Spa.update spaConfig msg_ model.spa
                |> Tuple.mapFirst (\spa -> { model | spa = spa })

        PageMsg msg_ ->
            case model.page of
                Just page ->
                    Page.update msg_ page
                        |> Tuple.mapFirst (\page_ -> { model | page = Just page_ })
                        |> Tuple.mapSecond (Cmd.map PageMsg)

                Nothing ->
                    ( model, Cmd.none )

        SampleAssetsReceived result ->
            case result of
                Ok modelIndex ->
                    { model | sampleAssets = RemoteData.Success modelIndex }
                        |> updateWithRoute (Spa.currentRoute model.spa)

                Err error ->
                    ( { model | sampleAssets = RemoteData.Failure error }
                    , Cmd.none
                    )

        ShowModal modal ->
            ( { model | modal = modal }
            , Ports.openDialog "modal"
            )

        ShowShareSheet ->
            ( model, Ports.share { title = "Elm glTF package" } )
