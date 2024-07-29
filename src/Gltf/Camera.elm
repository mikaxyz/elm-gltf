module Gltf.Camera exposing (Camera, Index(..), Projection(..), OrthographicCamera, PerspectiveCamera)

{-| A camera’s projection as defined in the [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-camera).

@docs Camera, Index, Projection, OrthographicCamera, PerspectiveCamera

-}


{-| Use this to get a [cameraByIndex](Gltf#cameraByIndex)
-}
type Index
    = Index Int


{-| A camera’s with a [Perspective or Orthographic projection](Gltf-Camera#Projection)
-}
type alias Camera =
    { name : Maybe String
    , index : Index
    , projection : Projection
    }


{-| Specifies if the camera uses a perspective or orthographic projection.
-}
type Projection
    = Perspective PerspectiveCamera
    | Orthographic OrthographicCamera


{-| A perspective camera containing properties to create a perspective projection matrix.
-}
type alias PerspectiveCamera =
    { yFov : Float
    , zNear : Float
    , aspectRatio : Maybe Float
    , zFar : Maybe Float
    }


{-| An orthographic camera containing properties to create an orthographic projection matrix.
-}
type alias OrthographicCamera =
    { xMag : Float
    , yMag : Float
    , zFar : Float
    , zNear : Float
    }
