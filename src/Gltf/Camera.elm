module Gltf.Camera exposing (Camera, Index(..), Projection(..), OrthographicCamera, PerspectiveCamera)

{-| TODO: Docs

@docs Camera, Index, Projection, OrthographicCamera, PerspectiveCamera

-}


{-| TODO: Docs
-}
type Index
    = Index Int


{-| TODO: Docs
-}
type alias Camera =
    { name : Maybe String
    , index : Index
    , projection : Projection
    }


{-| TODO: Docs
-}
type Projection
    = Perspective PerspectiveCamera
    | Orthographic OrthographicCamera


{-| TODO: Docs
-}
type alias PerspectiveCamera =
    { yFov : Float
    , zNear : Float
    , aspectRatio : Maybe Float
    , zFar : Maybe Float
    }


{-| TODO: Docs
-}
type alias OrthographicCamera =
    { xMag : Float
    , yMag : Float
    , zFar : Float
    , zNear : Float
    }
