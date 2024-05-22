module SampleAssets exposing
    ( Asset
    , AssetId(..)
    , SampleAssets
    , decoder
    , getAsset
    , toBinaryIdentifier
    , toBinaryUrl
    , toList
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


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


toBinaryUrl : Asset -> Maybe String
toBinaryUrl asset =
    case asset.variants.binary of
        Just (Path path) ->
            [ "https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Assets/main/Models"
            , asset.name
            , "glTF-Binary"
            , path
            ]
                |> String.join "/"
                |> Just

        Nothing ->
            Nothing


toBinaryIdentifier : Asset -> Maybe String
toBinaryIdentifier asset =
    -- https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Assets/main/Models/Triangle/glTF-Embedded/Triangle.gltf
    case asset.variants.binary of
        Just (Path path) ->
            path
                |> String.replace ".glb" ""
                |> Just

        Nothing ->
            Nothing


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
