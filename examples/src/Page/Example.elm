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


loadBrdfLUTTexture : (Result WebGL.Texture.Error WebGL.Texture.Texture -> Msg) -> Cmd Msg
loadBrdfLUTTexture msg =
    WebGL.Texture.loadWith
        (WebGL.Texture.defaultOptions
            |> (\options ->
                    { options
                        | flipY = False
                        , magnify = WebGL.Texture.nearest
                        , minify = WebGL.Texture.linearMipmapLinear
                    }
               )
        )
        "/assets/brdfLUT.png"
        |> Task.attempt msg


loadEnvironmentTexture : (Result WebGL.Texture.Error WebGL.Texture.Texture -> Msg) -> Cmd Msg
loadEnvironmentTexture msg =
    WebGL.Texture.loadCubeMap
        { xPos = "/assets/papermill/diffuse/diffuse_right_0.jpg"
        , xNeg = "/assets/papermill/diffuse/diffuse_left_0.jpg"
        , yPos = "/assets/papermill/diffuse/diffuse_bottom_0.jpg"
        , yNeg = "/assets/papermill/diffuse/diffuse_top_0.jpg"
        , zPos = "/assets/papermill/diffuse/diffuse_front_0.jpg"
        , zNeg = "/assets/papermill/diffuse/diffuse_back_0.jpg"
        }
        |> Task.attempt msg


loadSpecularEnvironmentTexture : (Result WebGL.Texture.Error WebGL.Texture.Texture -> Msg) -> Cmd Msg
loadSpecularEnvironmentTexture msg =
    WebGL.Texture.loadCubeMap
        { xPos = "/assets/hospital_room_2/px.png"
        , xNeg = "/assets/hospital_room_2/nx.png"
        , yPos = "/assets/hospital_room_2/ny.png"
        , yNeg = "/assets/hospital_room_2/py.png"
        , zPos = "/assets/hospital_room_2/pz.png"
        , zNeg = "/assets/hospital_room_2/nz.png"
        }
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
        , loadEnvironmentTexture EnvironmentTextureReceived
        , loadSpecularEnvironmentTexture SpecularEnvironmentTextureReceived
        , loadBrdfLUTTexture BrdfLUTTextureReceived
        ]
    )


initWithLocalAsset : String -> ( Model, Cmd Msg )
initWithLocalAsset path =
    ( Model.init (Model.Local path)
    , Cmd.batch
        [ Gltf.getEmbedded path GltfReceived
        , loadFallbackTexture FallbackTextureReceived
        , loadEnvironmentTexture EnvironmentTextureReceived
        , loadSpecularEnvironmentTexture SpecularEnvironmentTextureReceived
        , loadBrdfLUTTexture BrdfLUTTextureReceived
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
