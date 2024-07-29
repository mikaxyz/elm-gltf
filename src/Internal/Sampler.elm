module Internal.Sampler exposing
    ( Index(..)
    , Sampler
    , decoder
    , indexDecoder
    , magFilterToTextureOption
    , minFilterToTextureOption
    , wrapToTextureOption
    )

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import WebGL.Texture


type Index
    = Index Int


type alias Sampler =
    { name : Maybe String
    , magFilter : Maybe MagFilter
    , minFilter : Maybe MinFilter
    , wrapS : Wrap
    , wrapT : Wrap
    }


magFilterToTextureOption : MagFilter -> WebGL.Texture.Resize WebGL.Texture.Bigger
magFilterToTextureOption magFilter =
    case magFilter of
        MagNearest ->
            WebGL.Texture.nearest

        MagLinear ->
            WebGL.Texture.linear


minFilterToTextureOption : MinFilter -> WebGL.Texture.Resize WebGL.Texture.Smaller
minFilterToTextureOption minFilter =
    case minFilter of
        MinNearest ->
            WebGL.Texture.nearest

        MinLinear ->
            WebGL.Texture.linear

        NearestMipmapNearest ->
            WebGL.Texture.nearestMipmapNearest

        LinearMipmapNearest ->
            WebGL.Texture.linearMipmapNearest

        NearestMipmapLinear ->
            WebGL.Texture.nearestMipmapLinear

        LinearMipmapLinear ->
            WebGL.Texture.linearMipmapLinear


wrapToTextureOption : Wrap -> WebGL.Texture.Wrap
wrapToTextureOption wrap =
    case wrap of
        ClampToEdge ->
            WebGL.Texture.clampToEdge

        MirroredRepeat ->
            WebGL.Texture.mirroredRepeat

        Repeat ->
            WebGL.Texture.repeat


type MagFilter
    = MagNearest -- 9728
    | MagLinear -- 9729


type MinFilter
    = MinNearest
    | MinLinear
    | NearestMipmapNearest
    | LinearMipmapNearest
    | NearestMipmapLinear
    | LinearMipmapLinear


type Wrap
    = ClampToEdge
    | MirroredRepeat
    | Repeat


indexDecoder : JD.Decoder Index
indexDecoder =
    JD.int |> JD.map Index


decoder : JD.Decoder Sampler
decoder =
    JD.succeed Sampler
        |> JDP.optional "name" (JD.maybe JD.string) Nothing
        |> JDP.optional "magFilter" (JD.maybe magFilterDecoder) Nothing
        |> JDP.optional "minFilter" (JD.maybe minFilterDecoder) Nothing
        |> JDP.optional "wrapS" wrapDecoder Repeat
        |> JDP.optional "wrapT" wrapDecoder Repeat


magFilterDecoder : JD.Decoder MagFilter
magFilterDecoder =
    JD.int
        |> JD.andThen
            (\mag ->
                case mag of
                    9728 ->
                        JD.succeed MagNearest

                    9729 ->
                        JD.succeed MagLinear

                    m ->
                        JD.fail <| "Sampler mag filter " ++ String.fromInt m ++ " is unknown"
            )


minFilterDecoder : JD.Decoder MinFilter
minFilterDecoder =
    JD.int
        |> JD.andThen
            (\min ->
                case min of
                    9728 ->
                        JD.succeed MinNearest

                    9729 ->
                        JD.succeed MinLinear

                    9984 ->
                        JD.succeed NearestMipmapNearest

                    9985 ->
                        JD.succeed LinearMipmapNearest

                    9986 ->
                        JD.succeed NearestMipmapLinear

                    9987 ->
                        JD.succeed LinearMipmapLinear

                    m ->
                        JD.fail <| "Sampler min filter " ++ String.fromInt m ++ " is unknown"
            )


wrapDecoder : JD.Decoder Wrap
wrapDecoder =
    JD.int
        |> JD.andThen
            (\wrap ->
                case wrap of
                    33071 ->
                        JD.succeed ClampToEdge

                    33648 ->
                        JD.succeed MirroredRepeat

                    10497 ->
                        JD.succeed Repeat

                    w ->
                        JD.fail <| "Sampler wrap " ++ String.fromInt w ++ " is unknown"
            )
