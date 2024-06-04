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
import Task exposing (Task)
import WebGL.Texture
import XYZMika.Dragon as Dragon


type alias Msg =
    Model.Msg


type alias Model =
    Model.Model


loadFallbackTexture : (Result WebGL.Texture.Error WebGL.Texture.Texture -> Msg) -> Cmd Msg
loadFallbackTexture msg =
    WebGL.Texture.load "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAE0lEQVR4nA3AAQEAAABAIP6fJlkACgACy3XdIwAAAABJRU5ErkJggg=="
        |> Task.attempt msg


initWithSampleAsset : SampleAssets.Asset -> ( Model, Cmd Msg )
initWithSampleAsset asset =
    ( Model.init (Model.SampleAsset asset)
    , Cmd.batch
        [ SampleAssets.toBinaryUrl asset
            |> Maybe.map
                (\url ->
                    Gltf.getBinary url GltfReceived
                )
            |> Maybe.withDefault Cmd.none
        , loadFallbackTexture FallbackTextureReceived
        ]
    )


initWithLocalAsset : String -> ( Model, Cmd Msg )
initWithLocalAsset path =
    ( Model.init (Model.Local path)
    , Cmd.batch
        [ Gltf.getEmbedded path GltfReceived
        , loadFallbackTexture FallbackTextureReceived
        ]
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
