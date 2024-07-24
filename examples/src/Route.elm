module Route exposing (Route(..), fromUrl, href, toPath)

import Html
import Html.Attributes
import SampleAssets
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser)


type Route
    = Root
    | Example SampleAssets.SampleType SampleAssets.AssetId


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Root Url.Parser.top
        , Url.Parser.map (SampleAssets.AssetId >> Example SampleAssets.Default) (Url.Parser.s "default" </> Url.Parser.string)
        , Url.Parser.map (SampleAssets.AssetId >> Example SampleAssets.Binary) (Url.Parser.s "binary" </> Url.Parser.string)
        , Url.Parser.map (SampleAssets.AssetId >> Example SampleAssets.Embedded) (Url.Parser.s "embedded" </> Url.Parser.string)
        ]


href : Route -> Html.Attribute msg
href route_ =
    Html.Attributes.href (toPath route_)


toPath : Route -> String
toPath route =
    case route of
        Root ->
            "/"

        Example SampleAssets.Default (SampleAssets.AssetId example) ->
            "/default/" ++ example

        Example SampleAssets.Binary (SampleAssets.AssetId example) ->
            "/binary/" ++ example

        Example SampleAssets.Embedded (SampleAssets.AssetId example) ->
            "/embedded/" ++ example


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
