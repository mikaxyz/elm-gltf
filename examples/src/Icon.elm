module Icon exposing (..)

import Html exposing (Html, i)
import Html.Attributes exposing (class)


home : Html msg
home =
    i [ class "bx bxs-planet" ] []


share : Html msg
share =
    i [ class "bx bxs-mobile-vibration" ] []


help : Html msg
help =
    i [ class "bx bxs-bot" ] []


x : Html msg
x =
    i [ class "bx bx-x" ] []
