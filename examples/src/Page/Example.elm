module Page.Example exposing
    ( Model
    , Msg
    , initWithLocalAsset
    , initWithSampleAsset
    , sampleAsset
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Gltf
import Html exposing (Html)
import Keyboard
import Page.Example.Model as Model exposing (Model, Msg(..))
import Page.Example.Update as Update
import Page.Example.View as View
import SampleAssets
import XYZMika.Dragon as Dragon


type alias Msg =
    Model.Msg


type alias Model =
    Model.Model


initWithSampleAsset : SampleAssets.Asset -> ( Model, Cmd Msg )
initWithSampleAsset asset =
    ( Model.init (Model.SampleAsset asset)
    , SampleAssets.toBinaryUrl asset
        |> Maybe.map
            (\url ->
                Gltf.getBinary url GltfReceived
            )
        |> Maybe.withDefault Cmd.none
    )


initWithLocalAsset : String -> ( Model, Cmd Msg )
initWithLocalAsset path =
    ( Model.init (Model.Local path)
    , Gltf.getEmbedded path GltfReceived
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Update.update


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta
        , Browser.Events.onResize (\_ _ -> OnResize)
        , Keyboard.subscriptions { tagger = KeyboardMsg, keyDown = OnKeyDown }
        , Dragon.subscriptions model.dragon |> Sub.map DragonMsg
        ]


view : Model -> Html Msg
view =
    View.view


sampleAsset : Model -> Maybe SampleAssets.Asset
sampleAsset model =
    case model.asset of
        Model.Local _ ->
            Nothing

        Model.SampleAsset asset ->
            Just asset
