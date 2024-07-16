module Internal.Node exposing
    ( Index(..)
    , Node(..)
    , decoder
    , indexDecoder
    )

import Gltf.Query.Camera
import Gltf.Query.Transform exposing (Transform(..))
import Internal.Camera as Camera
import Internal.Mesh as Mesh
import Internal.Skin as Skin
import Internal.Util as Util
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Quaternion exposing (Quaternion)


type Index
    = Index Int


type Node
    = Node Data


decoder : Int -> JD.Decoder Node
decoder i =
    JD.map6 (Data (Index i))
        (Util.optionalField "name" (JD.maybe JD.string) Nothing)
        (JD.field "mesh" Mesh.indexDecoder |> JD.maybe)
        (JD.field "skin" Skin.indexDecoder |> JD.maybe)
        (JD.field "camera" Camera.indexDecoder |> JD.maybe)
        (Util.optionalField "children" (JD.list indexDecoder) [])
        (JDP.custom transformDecoder (JD.succeed identity))
        |> JD.map Node


transformDecoder : JD.Decoder Transform
transformDecoder =
    JD.oneOf
        [ JDP.required "matrix" mat4decoder (JD.succeed Matrix)
        , JD.map3
            (\rotation translation scale ->
                TRS
                    { rotation = rotation
                    , translation = translation
                    , scale = scale
                    }
            )
            (JD.field "rotation" quaternionDecoder |> JD.maybe)
            (JD.field "translation" vec3Decoder |> JD.maybe)
            (JD.field "scale" vec3Decoder |> JD.maybe)
        ]


mat4decoder : JD.Decoder Mat4
mat4decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    [ m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 ] ->
                        Mat4.fromRecord
                            { m11 = m11
                            , m21 = m21
                            , m31 = m31
                            , m41 = m41
                            , m12 = m12
                            , m22 = m22
                            , m32 = m32
                            , m42 = m42
                            , m13 = m13
                            , m23 = m23
                            , m33 = m33
                            , m43 = m43
                            , m14 = m14
                            , m24 = m24
                            , m34 = m34
                            , m44 = m44
                            }
                            |> JD.succeed

                    _ ->
                        JD.fail <| "Can not decode Mat4 from: [" ++ (values |> List.map String.fromFloat |> String.join ",") ++ "]"
            )


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


type alias Data =
    { index : Index
    , name : Maybe String
    , meshIndex : Maybe Mesh.Index
    , skinIndex : Maybe Skin.Index
    , cameraIndex : Maybe Gltf.Query.Camera.Index
    , children : List Index

    --
    , transform : Transform
    }


vec3Decoder : JD.Decoder Vec3
vec3Decoder =
    JD.list JD.float
        |> JD.andThen
            (\values ->
                case values of
                    x :: y :: z :: [] ->
                        JD.succeed (Vec3.vec3 x y z)

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
                        JD.succeed (Quaternion.quaternion w x y z)

                    _ ->
                        JD.fail <| "Failed to decode Quaternion " ++ (values |> List.map String.fromFloat |> String.join ",")
            )
