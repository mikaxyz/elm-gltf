module Internal.Node exposing
    ( Index(..)
    , Node(..)
    , decoder
    , indexDecoder
    )

import Internal.Camera as Camera
import Internal.Mesh as Mesh
import Internal.Skin as Skin
import Internal.Util as Util
import Json.Decode as JD


type Index
    = Index Int


type Node
    = Node Data


type alias Quaternion =
    { x : Float
    , y : Float
    , z : Float
    , w : Float
    }


type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


decoder : Int -> JD.Decoder Node
decoder i =
    JD.map8 (Data (Index i))
        (Util.optionalField "name" (JD.maybe JD.string) Nothing)
        (JD.field "mesh" Mesh.indexDecoder |> JD.maybe)
        (JD.field "skin" Skin.indexDecoder |> JD.maybe)
        (JD.field "camera" Camera.indexDecoder |> JD.maybe)
        (Util.optionalField "children" (JD.list indexDecoder) [])
        (JD.field "rotation" quaternionDecoder |> JD.maybe)
        (JD.field "translation" vec3Decoder |> JD.maybe)
        (JD.field "scale" vec3Decoder |> JD.maybe)
        |> JD.map Node


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


type alias Data =
    { index : Index
    , name : Maybe String
    , meshIndex : Maybe Mesh.Index
    , skinIndex : Maybe Skin.Index
    , cameraIndex : Maybe Camera.Index
    , children : List Index

    --
    , rotation : Maybe Quaternion
    , translation : Maybe Vec3
    , scale : Maybe Vec3
    }


vec3Decoder : JD.Decoder Vec3
vec3Decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    x :: y :: z :: [] ->
                        JD.succeed (Vec3 x y z)

                    _ ->
                        JD.fail <| "Failed to decode Vec3 " ++ (values |> List.map String.fromFloat |> String.join ",")
            )


quaternionDecoder : JD.Decoder Quaternion
quaternionDecoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    x :: y :: z :: w :: [] ->
                        JD.succeed (Quaternion x y z w)

                    _ ->
                        JD.fail <| "Failed to decode Quaternion " ++ (values |> List.map String.fromFloat |> String.join ",")
            )
