module Common exposing
    ( accessorAtIndex
    , bufferInfo
    , bufferViewAtIndex
    , imageAtIndex
    , materialAtIndex
    , maybeNodeTree
    , meshAtIndex
    , nodeAtIndex
    , samplerAtIndex
    , sceneAtIndex
    , textureAtIndex
    )

import Array
import Gltf.Query.Buffer exposing (Buffer)
import Gltf.Query.BufferStore as BufferStore exposing (BufferStore)
import Internal.Accessor as Accessor exposing (Accessor)
import Internal.BufferView as BufferView exposing (BufferView)
import Internal.Gltf exposing (Gltf)
import Internal.Image as Image exposing (Image)
import Internal.Material as Internal
import Internal.Mesh as Mesh
import Internal.Node as Node exposing (Node)
import Internal.Sampler
import Internal.Scene as Scene exposing (Scene)
import Internal.Texture
import Tree exposing (Tree)


sceneAtIndex : Gltf -> Scene.Index -> Maybe Scene
sceneAtIndex gltf (Scene.Index index) =
    gltf.scenes |> Array.get index


nodeAtIndex : Gltf -> Node.Index -> Maybe Node
nodeAtIndex gltf (Node.Index index) =
    gltf.nodes |> Array.get index


accessorAtIndex : Gltf -> Accessor.Index -> Maybe Accessor
accessorAtIndex gltf (Accessor.Index index) =
    gltf.accessors |> Array.get index


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


bufferInfo : Gltf -> BufferStore -> Accessor -> Maybe ( Accessor, BufferView, Buffer )
bufferInfo gltf bufferStore accessor =
    bufferViewAtIndex gltf accessor.bufferView
        |> Maybe.andThen
            (\bufferView ->
                bufferStore
                    |> BufferStore.get bufferView.buffer
                    |> Maybe.map (\buffer -> ( accessor, bufferView, buffer ))
            )


maybeNodeTree : Gltf -> Node.Index -> Maybe (Tree Node)
maybeNodeTree gltf index =
    nodeAtIndex gltf index
        |> Maybe.map
            (\(Node.Node node_) ->
                node_.children
                    |> List.filterMap (maybeNodeTree gltf)
                    |> Tree.tree (Node.Node node_)
            )
