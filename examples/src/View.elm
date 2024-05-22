module View exposing (doc)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Model, Msg(..))
import Page exposing (Page)
import Page.Example
import RemoteData
import Route
import SampleAssets exposing (SampleAssets)


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm glTF Examples"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    case model.sampleAssets of
        RemoteData.NotAsked ->
            Html.text "Error"

        RemoteData.Loading ->
            Html.text "Loading"

        RemoteData.Failure _ ->
            Html.text "Error"

        RemoteData.Success modelIndex ->
            appView model modelIndex


appView : Model -> SampleAssets -> Html Msg
appView model sampleAssets =
    let
        currentAsset : Maybe SampleAssets.Asset
        currentAsset =
            case model.page of
                Just (Page.Example page) ->
                    Page.Example.sampleAsset page

                _ ->
                    Nothing
    in
    main_ [ class "app" ]
        [ div [ class "app__viewport" ]
            [ model.page |> Maybe.map pageView |> Maybe.withDefault (text "")
            ]
        , sampleAssetNavigationView currentAsset sampleAssets
        ]


pageView : Page -> Html Msg
pageView page =
    case page of
        Page.Error _ ->
            h1 [] [ text "error" ]

        Page.Example page_ ->
            Page.Example.view page_
                |> Html.map (Page.ExampleMsg >> PageMsg)


sampleAssetNavigationView : Maybe SampleAssets.Asset -> SampleAssets -> Html Msg
sampleAssetNavigationView currentAsset sampleAssets =
    section [ class "panel" ]
        [ header [ class "panel__header" ]
            [ h1 [] [ text "Sample Assets" ]
            , p [] [ text "Binary (.bin)" ]
            ]
        , section [ class "panel__content" ]
            [ sampleAssets
                |> SampleAssets.toList
                |> List.map
                    (\( assetId, asset ) ->
                        li [ classList [ ( "panel__current", currentAsset == Just asset ) ] ]
                            [ SampleAssets.toBinaryIdentifier asset
                                |> Maybe.map
                                    (\_ ->
                                        a
                                            [ Route.href (Route.ExampleGlb assetId) ]
                                            [ text asset.name ]
                                    )
                                |> Maybe.withDefault (span [] [ text asset.name ])
                            ]
                    )
                |> ul []
            ]
        ]
