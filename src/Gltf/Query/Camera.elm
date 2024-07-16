module Gltf.Query.Camera exposing
    ( Camera
    , Index(..)
    , OrthographicCamera
    , PerspectiveCamera
    , Projection(..)
    )


type Index
    = Index Int


type alias Camera =
    { name : Maybe String
    , index : Index
    , projection : Projection
    }


type Projection
    = Perspective PerspectiveCamera
    | Orthographic OrthographicCamera


type alias PerspectiveCamera =
    { yFov : Float
    , zNear : Float
    , aspectRatio : Maybe Float
    , zFar : Maybe Float
    }


type alias OrthographicCamera =
    { xMag : Float
    , yMag : Float
    , zFar : Float
    , zNear : Float
    }
