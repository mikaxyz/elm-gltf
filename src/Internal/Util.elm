module Internal.Util exposing (arrayWithIndexedItems, optionalField, vec3Decoder)

import Array exposing (Array)
import Json.Decode as JD
import Math.Vector3 as Vec3 exposing (Vec3)


optionalField : String -> JD.Decoder a -> a -> JD.Decoder a
optionalField field decoder default =
    JD.maybe (JD.field field decoder)
        |> JD.map (Maybe.withDefault default)


arrayWithIndexedItems : (Int -> JD.Decoder a) -> JD.Decoder (Array a)
arrayWithIndexedItems d =
    JD.list JD.value
        |> JD.map
            (\values ->
                values
                    |> List.indexedMap
                        (\index value ->
                            value
                                |> JD.decodeValue (d index)
                                |> Result.toMaybe
                        )
                    |> List.filterMap identity
                    |> Array.fromList
            )


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
