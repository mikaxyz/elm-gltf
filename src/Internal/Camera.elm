module Internal.Camera exposing
    ( decoder
    , indexDecoder
    )

import Gltf.Camera
    exposing
        ( Camera
        , Index(..)
        , OrthographicCamera
        , PerspectiveCamera
        , Projection(..)
        )
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : Int -> JD.Decoder Camera
decoder index =
    JD.field "type" JD.string
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "perspective" ->
                        JD.succeed Camera
                            |> JDP.optional "name" (JD.maybe JD.string) Nothing
                            |> JDP.hardcoded (Index index)
                            |> JDP.required "perspective" (perspectiveDecoder |> JD.map Perspective)

                    "orthographic" ->
                        JD.succeed Camera
                            |> JDP.optional "name" (JD.maybe JD.string) Nothing
                            |> JDP.hardcoded (Index index)
                            |> JDP.required "orthographic" (orthographicDecoder |> JD.map Orthographic)

                    _ ->
                        JD.fail <| "Unknown camera type: " ++ type_
            )


perspectiveDecoder : JD.Decoder PerspectiveCamera
perspectiveDecoder =
    JD.succeed PerspectiveCamera
        |> JDP.required "yfov" JD.float
        |> JDP.required "znear" JD.float
        |> JDP.optional "aspectRatio" (JD.maybe JD.float) Nothing
        |> JDP.optional "zfar" (JD.maybe JD.float) Nothing


orthographicDecoder : JD.Decoder OrthographicCamera
orthographicDecoder =
    JD.succeed OrthographicCamera
        |> JDP.required "xmag" JD.float
        |> JDP.required "ymag" JD.float
        |> JDP.required "zfar" JD.float
        |> JDP.required "znear" JD.float
