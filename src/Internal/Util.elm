module Internal.Util exposing (arrayWithIndexedItems, optionalField)

import Array exposing (Array)
import Json.Decode as JD


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
