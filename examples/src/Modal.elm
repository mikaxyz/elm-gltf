module Modal exposing (helpPage, view)

import Html exposing (Html, button, div, section, text)
import Html.Attributes as HA exposing (class, id)
import Icon
import Markdown.Parser
import Markdown.Renderer
import Model exposing (Msg)


view : Html Msg -> Html Msg
view content =
    Html.node "dialog"
        [ id "modal"
        , class "modal"
        ]
        [ Html.form [ class "modal__close", HA.method "dialog" ] [ button [] [ Icon.x ] ]
        , section [ class "modal__content" ] [ content ]
        ]


helpPage : List (Html msg)
helpPage =
    renderMarkdown """This is the examples for [Elm glTF package](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/latest/) for importing 3D scenes into Elm.

The goal is to match what the the official [glTF sample viewer](https://github.khronos.org/glTF-Sample-Viewer-Release/) does. Currently the package is missing a lot of functionality but since it does a lot of things already that other packages does not provide I decided to release it as is.

Please open an [issue on GitHub](https://github.com/mikaxyz/elm-gltf/issues) if you have suggestions or found a bug.

The glTF package is meant to be general and usable in for example [elm-3d-scene](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/) but I created these examples using my unfinished "game engine" called [elm-xyz](https://github.com/mikaxyz/elm-xyz) mostly because I knew I could make the changes needed to support as many things in glTF format as possible. If you use this in **elm-3d-scene** or other "environments" I am open to add documentation/examples showing that.
"""


renderMarkdown : String -> List (Html msg)
renderMarkdown markdown =
    case Markdown.Parser.parse markdown of
        Ok okAst ->
            case Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer okAst of
                Ok rendered ->
                    rendered

                Err _ ->
                    []

        Err _ ->
            []
