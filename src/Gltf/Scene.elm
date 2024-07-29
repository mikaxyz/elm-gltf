module Gltf.Scene exposing (Scene, Index(..))

{-| A summary of a scene returned by [Gltf.scenes](Gltf#scenes).

@docs Scene, Index

-}


{-| The identifier for a Skin
-}
type Index
    = Index Int


{-| A scene summary
-}
type alias Scene =
    { name : Maybe String
    , index : Index
    , default : Bool
    }
