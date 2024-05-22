module Xyz.Mika.Spa.Router exposing (Router, currentUrl, init, push, pushUrl, route, withUrl)

import Browser.Navigation exposing (Key)
import Url exposing (Url)


type Router route
    = State
        { key : Key
        , route : Maybe route
        , url : Url
        }


init : Url -> Key -> (Url -> Maybe route) -> Router route
init url key fromUrl_ =
    State
        { key = key
        , route = fromUrl_ url
        , url = url
        }


route : Router route -> Maybe route
route (State state) =
    state.route


currentUrl : Router route -> Url
currentUrl (State { url }) =
    url


pushUrl : Router route -> String -> Cmd msg
pushUrl (State router) url =
    Browser.Navigation.pushUrl router.key url


push : (Url -> Maybe route) -> (route -> String) -> Router route -> Url -> ( Router route, Cmd msg )
push fromUrl routeToPath (State router) url =
    case fromUrl url of
        Just route_ ->
            ( State { router | route = Just route_, url = url }
            , Browser.Navigation.pushUrl router.key (Url.toString url)
            )

        Nothing ->
            ( State { router | route = Nothing, url = url }
            , Browser.Navigation.pushUrl router.key (Url.toString url)
            )


withUrl : (Url -> Maybe route) -> Url -> Router route -> Router route
withUrl fromUrl url (State state) =
    State { state | route = fromUrl url, url = url }
