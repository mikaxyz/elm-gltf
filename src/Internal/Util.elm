module Internal.Util exposing (optionalField)

import Json.Decode as JD


optionalField : String -> JD.Decoder a -> a -> JD.Decoder a
optionalField field decoder default =
    JD.maybe (JD.field field decoder)
        |> JD.map (Maybe.withDefault default)
