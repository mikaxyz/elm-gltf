module Page exposing
    ( Error(..)
    , Msg(..)
    , Page(..)
    , fromRoute
    , subscriptions
    , update
    )

import Page.Example
import Page.Example.Model as Model
import Route exposing (Route)
import SampleAssets exposing (SampleAssets)


type Msg
    = ExampleMsg Page.Example.Msg


type Error
    = NotFound


type Page
    = Error Error
    | Example Page.Example.Model


fromRoute :
    { pushRoute : Route -> Cmd msg
    , assets : SampleAssets
    }
    -> (Msg -> msg)
    -> Maybe Route
    -> ( Page, Cmd msg )
fromRoute config tagger maybeRoute =
    case maybeRoute of
        Just route ->
            case route of
                Route.Root ->
                    Page.Example.initWithLocalAsset "/assets/Zombie.glb" Model.Zombie
                        |> Tuple.mapFirst Example
                        |> Tuple.mapSecond (Cmd.map (ExampleMsg >> tagger))

                Route.Example sampleType assetId ->
                    config.assets
                        |> SampleAssets.getAsset assetId
                        |> Maybe.map
                            (\asset ->
                                Page.Example.initWithSampleAsset sampleType asset
                                    |> Tuple.mapFirst Example
                                    |> Tuple.mapSecond (Cmd.map (ExampleMsg >> tagger))
                            )
                        |> Maybe.withDefault ( Error NotFound, Cmd.none )

        Nothing ->
            ( Error NotFound, Cmd.none )


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Example model ->
            Page.Example.subscriptions model |> Sub.map ExampleMsg

        Error _ ->
            Sub.none


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case page of
        Example model ->
            case msg of
                ExampleMsg msg_ ->
                    Page.Example.update msg_ model
                        |> Tuple.mapFirst Example
                        |> Tuple.mapSecond (Cmd.map ExampleMsg)

        Error _ ->
            ( page
            , Cmd.none
            )
