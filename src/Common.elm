module Common exposing
    ( accessorAtIndex
    , bufferAtIndex
    , bufferInfo
    , bufferViewAtIndex
    , imageAtIndex
    , materialAtIndex
    , meshAtIndex
    , nodeAtIndex
    , samplerAtIndex
    , sceneAtIndex
    , textureAtIndex
    )

import Array
import Gltf exposing (Gltf)
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.Buffer as Buffer exposing (Buffer)
import Internal.BufferView as BufferView exposing (BufferView)
import Internal.Image as Image exposing (Image)
import Internal.Material as Internal
import Internal.Mesh as Mesh
import Internal.Node as Node exposing (Node)
import Internal.Sampler
import Internal.Scene as Scene exposing (Scene)
import Internal.Texture


sceneAtIndex : Gltf -> Scene.Index -> Maybe Scene
sceneAtIndex gltf (Scene.Index index) =
    gltf.scenes |> Array.get index


nodeAtIndex : Gltf -> Node.Index -> Maybe Node
nodeAtIndex gltf (Node.Index index) =
    gltf.nodes |> Array.get index


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


bufferAtIndex : Gltf -> Buffer.Index -> Maybe Buffer
bufferAtIndex gltf (Buffer.Index index) =
    gltf.buffers |> Array.get index


bufferViewAtIndex : Gltf -> BufferView.Index -> Maybe BufferView
bufferViewAtIndex gltf (BufferView.Index index) =
    gltf.bufferViews |> Array.get index


meshAtIndex : Gltf -> Mesh.Index -> Maybe Mesh.Mesh
meshAtIndex gltf (Mesh.Index index) =
    gltf.meshes |> Array.get index


imageAtIndex : Gltf -> Image.Index -> Maybe Image
imageAtIndex gltf (Image.Index index) =
    gltf.images |> Array.get index


materialAtIndex : Gltf -> Internal.Index -> Maybe Internal.Material
materialAtIndex gltf (Internal.Index index) =
    gltf.materials |> Array.get index


textureAtIndex : Gltf -> Internal.Texture.Index -> Maybe Internal.Texture.Texture
textureAtIndex gltf (Internal.Texture.Index index) =
    gltf.textures |> Array.get index


samplerAtIndex : Gltf -> Internal.Sampler.Index -> Maybe Internal.Sampler.Sampler
samplerAtIndex gltf (Internal.Sampler.Index index) =
    gltf.samplers |> Array.get index


bufferInfo : Gltf -> Accessor -> Maybe ( Accessor, BufferView, Buffer )
bufferInfo gltf accessor =
    Maybe.map2 (\bufferView buffer -> ( accessor, bufferView, buffer ))
        (bufferViewAtIndex gltf accessor.bufferView)
        (readBuffer gltf accessor)


readBuffer : Gltf -> Accessor -> Maybe Buffer
readBuffer gltf accessor =
    let
        maybeBufferView : BufferView.Index -> Maybe BufferView
        maybeBufferView x =
            bufferViewAtIndex gltf x

        maybeBuffer : BufferView -> Maybe Buffer
        maybeBuffer { buffer } =
            bufferAtIndex gltf buffer
    in
    accessor.bufferView
        |> maybeBufferView
        |> Maybe.andThen maybeBuffer
