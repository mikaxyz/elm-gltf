module Main exposing (main)

import Http
import Model exposing (Model, Msg(..))
import Page
import Route exposing (Route)
import SampleAssets
import Update exposing (update)
import View
import Xyz.Mika.Spa as Spa exposing (Spa)


main : Program () Model Msg
main =
    Spa.application Update.spaConfig
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = View.doc
        , tagger = SpaMsg
        }


init : () -> ( Spa Route, Cmd Msg ) -> ( Model, Cmd Msg )
init () ( spa, spaCmd ) =
    ( Model.init spa
    , Cmd.batch
        [ spaCmd
        , Http.get
            { url = "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Assets/main/Models/model-index.Khronos.json"
            , expect = Http.expectJson SampleAssetsReceived SampleAssets.decoder
            }
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.page
        |> Maybe.map (Page.subscriptions >> Sub.map PageMsg)
        |> Maybe.withDefault Sub.none
