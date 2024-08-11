module Xyz.Mika.Dragon exposing
    ( Dragon
    , Msg
    , Vector
    , dragEvents
    , init
    , isDragging
    , subscriptions
    , update
    )

import Browser.Events
import Html
import Html.Events
import Html.Events.Extra.Touch as Touch
import Json.Decode as JD
import Math.Vector2 as Vec2 exposing (Vec2)
import Task


type Dragon
    = Dragon
        { drag : Maybe Drag
        , current : Vec2
        , clickSensitivity : Float
        }


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Drag =
    { from : Vec2
    , to : Vec2
    }


type Msg
    = DragStart Vec2
    | DragUpdate Vec2
    | DragEnd Vec2


init : Dragon
init =
    Dragon
        { drag = Nothing
        , current = Vec2.vec2 0 0
        , clickSensitivity = 1.0
        }


isDragging : Dragon -> Bool
isDragging (Dragon { drag }) =
    drag /= Nothing


update : { tagger : Msg -> msg, onDragUpdate : Vector -> msg, onMouseUp : Vector -> msg } -> Msg -> Dragon -> ( Dragon, Cmd msg )
update { onDragUpdate, onMouseUp } msg (Dragon dragon) =
    case msg of
        DragStart position ->
            ( Dragon { dragon | drag = Just { from = position, to = position } }
            , Cmd.none
            )

        DragUpdate position ->
            case dragon.drag of
                Just drag ->
                    ( Dragon { dragon | drag = Just { drag | to = position } }
                    , Task.succeed (Vec2.sub position drag.to |> Vec2.toRecord)
                        |> Task.perform onDragUpdate
                    )

                Nothing ->
                    ( Dragon dragon, Cmd.none )

        DragEnd position ->
            ( Dragon
                { dragon
                    | drag = Nothing
                    , current = dragon.current |> Vec2.add position
                }
            , case clickPositionWhenDragDistanceBelow dragon.clickSensitivity dragon.drag of
                Just clickPosition ->
                    Task.succeed clickPosition |> Task.perform onMouseUp

                Nothing ->
                    Cmd.none
            )


clickPositionWhenDragDistanceBelow : Float -> Maybe Drag -> Maybe Vector
clickPositionWhenDragDistanceBelow limit drag =
    drag
        |> Maybe.andThen
            (\{ from, to } ->
                if Vec2.distance from to < limit then
                    Just (Vec2.toRecord to)

                else
                    Nothing
            )


subscriptions : Dragon -> Sub Msg
subscriptions (Dragon dragon) =
    case dragon.drag of
        Just drag ->
            Sub.batch
                [ Browser.Events.onMouseUp (vectorDecoder |> JD.map (\end -> DragEnd (drag.from |> Vec2.sub end)))
                , Browser.Events.onMouseMove (vectorDecoder |> JD.map DragUpdate)
                ]

        Nothing ->
            Sub.none


dragEvents : (Msg -> msg) -> List (Html.Attribute msg)
dragEvents tagger =
    [ Html.Events.on "mousedown" (vectorDecoder |> JD.map (DragStart >> tagger))
    , Touch.onStart (touchCoordinates >> DragStart >> tagger)
    , Touch.onMove (touchCoordinates >> DragUpdate >> tagger)
    , Touch.onEnd (touchCoordinates >> DragEnd >> tagger)
    ]


touchCoordinates : Touch.Event -> Vec2
touchCoordinates touchEvent =
    List.head touchEvent.targetTouches
        |> Maybe.map (\{ clientPos } -> Vec2.vec2 (Tuple.first clientPos) (Tuple.second clientPos))
        |> Maybe.withDefault (Vec2.vec2 0 0)


vectorDecoder : JD.Decoder Vec2
vectorDecoder =
    JD.map2 Vec2.vec2
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
