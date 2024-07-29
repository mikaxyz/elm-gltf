module View exposing (doc)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon
import Modal
import Model exposing (Model, Msg(..))
import Page exposing (Page)
import Page.Example
import RemoteData
import Route
import SampleAssets exposing (SampleAssets)


doc : Model -> Browser.Document Msg
doc model =
    { title = "Elm glTF Examples"
    , body =
        [ view model
        , Modal.view (modalView model.modal)
        ]
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
        , sampleAssetNavigationView model.sampleAssetsVisible currentAsset model.sampleType sampleAssets
        , navigationView
        ]


modalView : Maybe Model.Modal -> Html Msg
modalView modal =
    case modal of
        Just Model.Help ->
            Modal.helpPage |> div []

        Nothing ->
            text ""


navigationView : Html Msg
navigationView =
    let
        link : { url : String, icon : Html msg, label : String } -> Html msg
        link { url, icon, label } =
            a [ href url ] [ icon, span [] [ text label ] ]

        action : { msg : msg, icon : Html msg, label : String } -> Html msg
        action { msg, icon, label } =
            button [ onClick msg ] [ icon, span [] [ text label ] ]
    in
    nav [ class "app__navigation" ]
        [ [ link { url = "/", icon = Icon.home, label = "Home" }
          , action { msg = ToggleSampleAssets, icon = Icon.collection, label = "Sample Assets" }
          , action { msg = ShowModal (Just Model.Help), icon = Icon.help, label = "About" }
          , action { msg = ShowShareSheet, icon = Icon.share, label = "Share" }
          ]
            |> List.map (\x -> li [] [ x ])
            |> ul []
        ]


pageView : Page -> Html Msg
pageView page =
    case page of
        Page.Error _ ->
            h1 [] [ text "error" ]

        Page.Example page_ ->
            Page.Example.view page_
                |> Html.map (Page.ExampleMsg >> PageMsg)


sampleAssetNavigationView : Bool -> Maybe SampleAssets.Asset -> SampleAssets.SampleType -> SampleAssets -> Html Msg
sampleAssetNavigationView isOpen currentAsset sampleType sampleAssets =
    section [ class "panel", classList [ ( "panel--open", isOpen ) ] ]
        [ header [ class "panel__header" ]
            [ h1 [] [ text "Sample Assets" ]
            , button [ class "panel__close", onClick ToggleSampleAssets ] [ Icon.x ]
            , sampleTypeSelector sampleType
            ]
        , section [ class "panel__content" ]
            [ sampleAssets
                |> SampleAssets.toList
                |> List.map
                    (\( assetId, asset ) ->
                        li [ classList [ ( "panel__current", currentAsset == Just asset ) ] ]
                            [ SampleAssets.toUrl sampleType asset
                                |> Maybe.map
                                    (\_ ->
                                        a
                                            [ Route.href <| Route.Example sampleType assetId ]
                                            [ text asset.name ]
                                    )
                                |> Maybe.withDefault (span [] [ text asset.name ])
                            ]
                    )
                |> ul []
            ]
        ]


sampleTypeSelector : SampleAssets.SampleType -> Html Msg
sampleTypeSelector sampleType =
    let
        btn : String -> SampleAssets.SampleType -> Html Msg
        btn label sampleType_ =
            button
                [ onClick <| UserClickedSampleType sampleType_
                , class "selector__button"
                , classList [ ( "selector__button--active", sampleType == sampleType_ ) ]
                ]
                [ span [ class "selector__label" ] [ text label ] ]
    in
    nav [ class "selector" ]
        [ btn "Default" SampleAssets.Default
        , btn "Binary" SampleAssets.Binary
        , btn "Embedded" SampleAssets.Embedded
        ]
