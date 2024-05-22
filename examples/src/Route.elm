module Route exposing (Route(..), fromUrl, href, toPath)

import Html
import Html.Attributes
import SampleAssets
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser)


type Route
    = Root
    | ExampleGlb SampleAssets.AssetId


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map (SampleAssets.AssetId >> ExampleGlb) (Url.Parser.s "glb" </> Url.Parser.string)
        ]


href : Route -> Html.Attribute msg
href route_ =
    Html.Attributes.href (toPath route_)


toPath : Route -> String
toPath route =
    case route of
        Root ->
            "/"

        ExampleGlb (SampleAssets.AssetId example) ->
            "/glb/" ++ example


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
