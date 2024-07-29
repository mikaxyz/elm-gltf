module SampleAssets exposing
    ( Asset
    , AssetId(..)
    , SampleAssets
    , SampleType(..)
    , decoder
    , getAsset
    , toList
    , toUrl
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


type SampleType
    = Default
    | Binary
    | Embedded


type SampleAssets
    = SampleAssets (Dict String Asset)


type AssetId
    = AssetId String


type alias Asset =
    { label : String
    , name : String
    , screenshot : Path
    , tags : List Tag
    , variants : Variants
    }


type alias Variants =
    { default : Maybe Path
    , binary : Maybe Path
    , embedded : Maybe Path
    }


type Path
    = Path String


type Tag
    = Tag String


toList : SampleAssets -> List ( AssetId, Asset )
toList (SampleAssets x) =
    Dict.toList x |> List.map (Tuple.mapFirst AssetId)


getAsset : AssetId -> SampleAssets -> Maybe Asset
getAsset (AssetId id) (SampleAssets assets) =
    Dict.get id assets


toUrl : SampleType -> Asset -> Maybe String
toUrl type_ asset =
    let
        toAssetUrl (Path path) =
            [ "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Assets/main/Models"
            , asset.name
            , case type_ of
                Default ->
                    "glTF"

                Binary ->
                    "glTF-Binary"

                Embedded ->
                    "glTF-Embedded"
            , path
            ]
                |> String.join "/"
    in
    case type_ of
        Default ->
            asset.variants.default
                |> Maybe.map toAssetUrl

        Binary ->
            asset.variants.binary
                |> Maybe.map toAssetUrl

        Embedded ->
            asset.variants.embedded
                |> Maybe.map toAssetUrl


decoder : JD.Decoder SampleAssets
decoder =
    JD.list (assetDecoder |> JD.map (\asset -> ( asset.name, asset )))
        |> JD.map (Dict.fromList >> SampleAssets)


assetDecoder : JD.Decoder Asset
assetDecoder =
    JD.succeed Asset
        |> JDP.required "label" JD.string
        |> JDP.required "name" JD.string
        |> JDP.required "screenshot" pathDecoder
        |> JDP.required "tags" (JD.list tagDecoder)
        |> JDP.required "variants" variantsDecoder


pathDecoder =
    JD.string |> JD.map Path


tagDecoder =
    JD.string |> JD.map Tag


variantsDecoder =
    JD.succeed Variants
        |> JDP.optional "glTF" (JD.nullable pathDecoder) Nothing
        |> JDP.optional "glTF-Binary" (JD.nullable pathDecoder) Nothing
        |> JDP.optional "glTF-Embedded" (JD.nullable pathDecoder) Nothing
