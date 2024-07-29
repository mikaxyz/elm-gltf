module Xyz.Mika.Spa exposing
    ( Config
    , Msg
    , Spa
    , application
    , currentRoute
    , currentUrl
    , navigate
    , pushRoute
    , pushUrl
    , update
    )

import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key)
import Task
import Url exposing (Url)
import Xyz.Mika.Spa.Router as Router exposing (Router)


type Spa route
    = Spa { router : Router route }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


type alias Config route msg =
    { routeFromUrl : Url -> Maybe route
    , routeToPath : route -> String
    , onRouteChange : Maybe route -> msg
    , onHistoryChange : route -> msg
    }


application :
    Config route msg
    ->
        { init : flags -> ( Spa route, Cmd msg ) -> ( model, Cmd msg )
        , view : model -> Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , tagger : Msg -> msg
        }
    -> Program flags model msg
application config app =
    Browser.application
        { init =
            \flags url key ->
                init config.onRouteChange config.routeFromUrl url key
                    |> app.init flags
        , update = app.update
        , subscriptions = app.subscriptions
        , view = app.view
        , onUrlRequest = OnUrlRequest >> app.tagger
        , onUrlChange = OnUrlChange >> app.tagger
        }


currentUrl : Spa route -> Url
currentUrl (Spa { router }) =
    Router.currentUrl router


currentRoute : Spa route -> Maybe route
currentRoute (Spa { router }) =
    Router.route router


pushUrl : Spa route -> String -> Cmd msg
pushUrl (Spa spa) url =
    Router.pushUrl spa.router url


pushRoute : Config route msg -> Spa route -> route -> Cmd msg
pushRoute config (Spa spa) route =
    Router.pushUrl spa.router (config.routeToPath route)


navigate : Config route msg -> route -> Cmd msg
navigate config route =
    config.routeToPath route
        |> Navigation.load



--pushRoute : Spa route -> route -> Cmd msg
--pushRoute  (Spa spa) route =
--    --Browser.Navigation.pushUrl state.key (routeToPath x)


init :
    (Maybe route -> msg)
    -> (Url -> Maybe route)
    -> Url
    -> Key
    -> ( Spa route, Cmd msg )
init onRouteChange fromUrl url key =
    ( Spa { router = Router.init url key fromUrl }
    , Task.perform (\_ -> onRouteChange (fromUrl url)) (Task.succeed ())
      --, Cmd.none
    )


update :
    Config route msg
    -> Msg
    -> Spa route
    -> ( Spa route, Cmd msg )
update config msg (Spa model) =
    case msg of
        OnUrlRequest request ->
            case request of
                Browser.Internal url ->
                    if Router.currentUrl model.router == url then
                        ( Spa model, Cmd.none )

                    else
                        Router.push config.routeFromUrl config.routeToPath model.router url
                            |> Tuple.mapFirst (\router -> Spa { model | router = router })
                            |> Tuple.mapSecond
                                (\cmd ->
                                    Cmd.batch
                                        [ cmd
                                        , Task.perform (\_ -> config.onRouteChange (config.routeFromUrl url)) (Task.succeed ())
                                        ]
                                )

                Browser.External href ->
                    ( Spa model
                    , Navigation.load href
                    )

        OnUrlChange url ->
            ( Spa { model | router = Router.withUrl config.routeFromUrl url model.router }
            , if Router.currentUrl model.router == url then
                Cmd.none

              else
                case config.routeFromUrl url of
                    Just route ->
                        Task.perform (\_ -> config.onHistoryChange route) (Task.succeed ())

                    Nothing ->
                        Cmd.none
            )
