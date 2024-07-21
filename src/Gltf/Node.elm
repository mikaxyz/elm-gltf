module Gltf.Node exposing (Node(..), Properties(..))

{-| TODO: Docs


# Types

@docs Node, Properties

-}

import Gltf.Camera
import Gltf.Mesh exposing (Mesh)
import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Skin
import Gltf.Transform exposing (Transform)


{-| TODO: Docs
-}
type Node
    = EmptyNode Properties
    | CameraNode Gltf.Camera.Index Properties
    | MeshNode (List Mesh) Properties
    | SkinnedMeshNode (List Mesh) Gltf.Skin.Index Properties


{-| TODO: Docs
-}
type Properties
    = Properties
        { nodeIndex : NodeIndex
        , nodeName : Maybe String
        , transform : Transform
        }
