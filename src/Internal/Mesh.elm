module Internal.Mesh exposing
    ( Attribute(..)
    , Index(..)
    , Mesh
    , Mode(..)
    , Primitive
    , decoder
    , indexDecoder
    )

import Dict
import Internal.Accessor as Accessor
import Internal.Material as Material
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type Index
    = Index Int


type alias Mesh =
    { primitives : List Primitive
    }


type alias Primitive =
    { attributes : List Attribute
    , indices : Maybe Accessor.Index
    , material : Maybe Material.Index
    , mode : Mode
    }


type Mode
    = Points
    | Lines
    | LineLoop
    | LineStrip
    | Triangles
    | TriangleStrip
    | TriangleFan


modeDecoder : JD.Decoder Mode
modeDecoder =
    JD.int
        |> JD.andThen
            (\mode ->
                case mode of
                    0 ->
                        JD.succeed Points

                    1 ->
                        JD.succeed Lines

                    2 ->
                        JD.succeed LineLoop

                    3 ->
                        JD.succeed LineStrip

                    4 ->
                        JD.succeed Triangles

                    5 ->
                        JD.succeed TriangleStrip

                    6 ->
                        JD.succeed TriangleFan

                    m ->
                        JD.fail <| "Unknown mesh.primitive.mode " ++ String.fromInt m
            )


type Attribute
    = Position Accessor.Index
    | Normal Accessor.Index
    | Tangent Accessor.Index
    | Color Int Accessor.Index
    | Joints Int Accessor.Index
    | Weights Int Accessor.Index
    | TexCoord Int Accessor.Index
    | Unknown String


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Mesh
decoder =
    JD.map Mesh
        (JD.field "primitives" (JD.list primitiveDecoder))


primitiveDecoder : JD.Decoder Primitive
primitiveDecoder =
    JD.succeed Primitive
        |> JDP.required "attributes" attributesDecoder
        |> JDP.optional "indices" (JD.maybe Accessor.indexDecoder) Nothing
        |> JDP.optional "material" (JD.maybe Material.indexDecoder) Nothing
        |> JDP.optional "mode" modeDecoder Triangles


attributesDecoder : JD.Decoder (List Attribute)
attributesDecoder =
    JD.dict Accessor.indexDecoder |> JD.map (Dict.map toAttribute >> Dict.values)


toAttribute : String -> Accessor.Index -> Attribute
toAttribute key accessorIndex =
    case key of
        "POSITION" ->
            Position accessorIndex

        "NORMAL" ->
            Normal accessorIndex

        "TANGENT" ->
            Tangent accessorIndex

        _ ->
            case String.split "_" key of
                "COLOR" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> Color index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                "JOINTS" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> Joints index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                "WEIGHTS" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> Weights index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                "TEXCOORD" :: indices :: [] ->
                    indices
                        |> String.toInt
                        |> Maybe.map (\index -> TexCoord index accessorIndex)
                        |> Maybe.withDefault (Unknown key)

                _ ->
                    Unknown key
