module Update exposing
    ( spaConfig
    , update
    )

import Model exposing (Model, Msg(..))
import Page
import Ports
import RemoteData
import Route exposing (Route)
import SampleAssets
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
    let
        sampleType : SampleAssets.SampleType
        sampleType =
            case maybeRoute of
                Just (Route.Example sampleAssets _) ->
                    sampleAssets

                _ ->
                    SampleAssets.Default
    in
    model.sampleAssets
        |> RemoteData.map
            (\assets ->
                Page.fromRoute
                    { pushRoute = Spa.pushRoute spaConfig model.spa
                    , assets = assets
                    }
                    PageMsg
                    maybeRoute
                    |> Tuple.mapFirst (\page -> { model | page = Just page, sampleType = sampleType })
            )
        |> RemoteData.withDefault ( { model | sampleType = sampleType }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRouteChange maybeRoute ->
            updateWithRoute maybeRoute model
                |> Tuple.mapFirst (\model_ -> { model_ | sampleAssetsVisible = False })

        OnHistoryChange route ->
            updateWithRoute (Just route) model

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

        ToggleSampleAssets ->
            ( { model | sampleAssetsVisible = not model.sampleAssetsVisible }, Cmd.none )

        UserClickedSampleType sampleType ->
            ( { model | sampleType = sampleType }, Cmd.none )

        ShowModal modal ->
            ( { model | modal = modal }
            , Ports.openDialog "modal"
            )

        ShowShareSheet ->
            ( model, Ports.share { title = "Elm glTF package" } )
