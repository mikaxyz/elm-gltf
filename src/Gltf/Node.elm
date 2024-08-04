module Gltf.Node exposing (Node(..), Properties(..))

{-| A Node is a container in a [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree#Tree) of nodes that makes up a "scene". A Node can just define a [Transform](Gltf-Transform#Transform) (meaning its children will appear in a different position) or a transform together with one or multiple [Meshes](Gltf-Mesh#Mesh) or a [Camera](Gltf-Camera#Camera). A single Node and its children can be controlled by an [Animation](Gltf-Animation#Animation)


# Types

@docs Node, Properties

-}

import Gltf.Camera
import Gltf.Mesh exposing (Mesh)
import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Skin
import Gltf.Transform exposing (Transform)


{-| The types of [Nodes](Gltf#nodeTrees) returned as a [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree#Tree) by a [query](Gltf#queries)
-}
type Node
    = EmptyNode Properties
    | CameraNode Gltf.Camera.Index Properties
    | MeshNode (List Mesh) Properties
    | SkinnedMeshNode (List Mesh) Gltf.Skin.Index Properties
    | BoneNode Gltf.Skin.Index (Maybe Float) Properties


{-| The [Transform](Gltf-Transform#Transform) of the Node along with name and index.
-}
type Properties
    = Properties
        { nodeIndex : NodeIndex
        , nodeName : Maybe String
        , transform : Transform
        }
