module Gltf.Query.Material exposing
    ( Material(..)
    , MaterialImage(..)
    , fromPrimitive
    )

import Base64.Encode
import Bytes exposing (Bytes)
import Bytes.Extra
import Common
import Gltf exposing (Gltf)
import Internal.Buffer exposing (Buffer(..))
import Internal.BufferView exposing (BufferView)
import Internal.Image as Image exposing (Image)
import Internal.Material as Internal
import Internal.Mesh exposing (Primitive)
import Internal.Texture exposing (Texture)
import Internal.TextureInfo exposing (TextureInfo)
import Math.Vector4 exposing (Vec4)


type Material
    = Material
        { name : Maybe String
        , index : Internal.Index
        , pbrMetallicRoughness : BbrMetallicRoughness
        }


type alias BbrMetallicRoughness =
    { baseColorFactor : Vec4
    , baseColorTexture : Maybe MaterialImage
    , metallicFactor : Float
    , roughnessFactor : Float
    }


type MaterialImage
    = Uri String
    | DataUri String


fromPrimitive : Gltf -> Primitive -> Maybe Material
fromPrimitive gltf primitive =
    case Maybe.map2 Tuple.pair primitive.material (primitive.material |> Maybe.andThen (Common.materialAtIndex gltf)) of
        Just ( index, material ) ->
            let
                baseColorTexture : Maybe Texture
                baseColorTexture =
                    material.pbrMetallicRoughness.baseColorTexture
                        |> Maybe.andThen (textureFromTextureInfo gltf)

                image : Maybe Image
                image =
                    baseColorTexture
                        |> Maybe.andThen .source
                        |> Maybe.andThen (Common.imageAtIndex gltf)

                materialImage : Maybe MaterialImage
                materialImage =
                    image |> Maybe.andThen (materialImageFromImage gltf)
            in
            Just (fromMaterial index material materialImage)

        Nothing ->
            Nothing


materialImageFromImage : Gltf -> Image -> Maybe MaterialImage
materialImageFromImage gltf image =
    case image.bufferView of
        Just bufferView ->
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

                base64 : Maybe String
                base64 =
                    bytes
                        |> Maybe.map (Base64.Encode.bytes >> Base64.Encode.encode)
            in
            case Maybe.map2 Tuple.pair image.mimeType base64 of
                Just ( mimeType, base64_ ) ->
                    case mimeType of
                        Image.Png ->
                            Just <| DataUri ("data:image/png;base64," ++ base64_)

                        Image.Jpeg ->
                            Just <| DataUri ("data:image/jpeg;base64," ++ base64_)

                Nothing ->
                    Nothing

        Nothing ->
            case image.uri of
                Just uri ->
                    Just (Uri uri)

                Nothing ->
                    Nothing


bytesFromBuffer : BufferView -> Buffer -> Bytes
bytesFromBuffer bufferView (Buffer buffer) =
    buffer
        |> Bytes.Extra.drop bufferView.byteOffset
        |> Bytes.Extra.take bufferView.byteLength


fromMaterial : Internal.Index -> Internal.Material -> Maybe MaterialImage -> Material
fromMaterial index material baseColorTexture =
    Material
        { name = material.name
        , index = index
        , pbrMetallicRoughness =
            { baseColorFactor = material.pbrMetallicRoughness.baseColorFactor
            , baseColorTexture = baseColorTexture
            , metallicFactor = material.pbrMetallicRoughness.metallicFactor
            , roughnessFactor = material.pbrMetallicRoughness.roughnessFactor
            }
        }


textureFromTextureInfo : Gltf -> TextureInfo -> Maybe Internal.Texture.Texture
textureFromTextureInfo gltf textureInfo =
    Common.textureAtIndex gltf textureInfo.index
