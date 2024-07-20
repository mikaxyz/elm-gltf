module Gltf.Query.Task exposing (loadTextureTask)

import Base64.Encode
import Bytes exposing (Bytes)
import Bytes.Extra
import Common
import Internal.Buffer exposing (Buffer(..))
import Internal.BufferView exposing (BufferView)
import Internal.Gltf exposing (Gltf)
import Internal.Image as Image exposing (Image)
import Internal.Sampler exposing (Sampler)
import Task exposing (Task)
import WebGL.Texture


loadTextureTask : Gltf -> Image -> Maybe Sampler -> Maybe (Task WebGL.Texture.Error WebGL.Texture.Texture)
loadTextureTask gltf image maybeSampler =
    case toLoadableImage gltf image of
        Just (DataUri dataUri) ->
            let
                defaultOptions : WebGL.Texture.Options
                defaultOptions =
                    WebGL.Texture.defaultOptions

                options : WebGL.Texture.Options
                options =
                    case maybeSampler of
                        Just sampler ->
                            -- flipY: https://github.com/KhronosGroup/glTF-Sample-Viewer/issues/16
                            { defaultOptions
                                | flipY = False
                                , magnify =
                                    sampler.magFilter
                                        |> Maybe.map Internal.Sampler.magFilterToTextureOption
                                        |> Maybe.withDefault defaultOptions.magnify
                                , minify =
                                    sampler.minFilter
                                        |> Maybe.map Internal.Sampler.minFilterToTextureOption
                                        |> Maybe.withDefault defaultOptions.minify
                                , horizontalWrap = Internal.Sampler.wrapToTextureOption sampler.wrapS
                                , verticalWrap = Internal.Sampler.wrapToTextureOption sampler.wrapT
                            }

                        Nothing ->
                            -- flipY: https://github.com/KhronosGroup/glTF-Sample-Viewer/issues/16
                            { defaultOptions | flipY = False }
            in
            Just (WebGL.Texture.loadWith options dataUri)

        Just (Uri _) ->
            Nothing

        Nothing ->
            Nothing


type LoadableImage
    = DataUri String
    | Uri String


toLoadableImage : Gltf -> Image -> Maybe LoadableImage
toLoadableImage gltf image =
    case image.uri of
        Just uri ->
            Just (Uri uri)

        Nothing ->
            case Maybe.map2 Tuple.pair image.mimeType image.bufferView of
                Just ( mimeType, bufferView ) ->
                    let
                        bufferView_ : Maybe BufferView
                        bufferView_ =
                            bufferView
                                |> Common.bufferViewAtIndex gltf

                        buffer_ : Maybe Buffer
                        buffer_ =
                            bufferView_
                                |> Maybe.andThen (\{ buffer } -> Common.bufferAtIndex gltf buffer)

                        bytes : Maybe Bytes
                        bytes =
                            Maybe.map2
                                bytesFromBuffer
                                bufferView_
                                buffer_
                    in
                    case bytes |> Maybe.map (Base64.Encode.bytes >> Base64.Encode.encode) of
                        Just base64 ->
                            case mimeType of
                                Image.Png ->
                                    Just <| DataUri ("data:image/png;base64," ++ base64)

                                Image.Jpeg ->
                                    Just <| DataUri ("data:image/jpeg;base64," ++ base64)

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing


bytesFromBuffer : BufferView -> Buffer -> Bytes
bytesFromBuffer bufferView (Buffer buffer) =
    buffer
        |> Bytes.Extra.drop bufferView.byteOffset
        |> Bytes.Extra.take bufferView.byteLength
