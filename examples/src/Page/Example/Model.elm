module Page.Example.Model exposing
    ( Asset(..)
    , DragTarget(..)
    , Error(..)
    , Model
    , Msg(..)
    , dragTarget
    , init
    , mapSceneOptions
    , viewportFromDomElement
    )

import Browser.Dom
import Gltf exposing (Gltf)
import Gltf.Animation exposing (Animation)
import Gltf.Camera
import Keyboard
import Page.Example.Material as Material
import Page.Example.Scene as Scene
import RemoteData exposing (RemoteData)
import SampleAssets
import WebGL.Texture
import XYZMika.Dragon as Dragon exposing (Dragon)
import XYZMika.XYZ
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Options as SceneOptions


maxResolution : number
maxResolution =
    1600


type Msg
    = OnAnimationFrameDelta Float
    | OnResize
      --
    | OnMouseUp Dragon.Vector
    | KeyboardMsg Keyboard.Msg
    | OnKeyDown Keyboard.Key
      --
    | DragonMsg Dragon.Msg
    | DragonOnDrag Dragon.Vector
      --
    | FallbackTextureReceived (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | EnvironmentTextureReceived (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | SpecularEnvironmentTextureReceived (Result WebGL.Texture.Error WebGL.Texture.Texture)
    | BrdfLUTTextureReceived (Result WebGL.Texture.Error WebGL.Texture.Texture)
      --
    | GltfMsg Gltf.Msg
    | GltfOnComplete (Result Gltf.Error Gltf.QueryResult)
      --
    | OnViewportElement (Result Browser.Dom.Error Browser.Dom.Element)
      --
    | UserSelectedCamera (Maybe Gltf.Camera.Index)
    | UserSelectedAnimation (Maybe Int)


type Asset
    = Local String
    | SampleAsset SampleAssets.Asset


type Error
    = TextureError WebGL.Texture.Error
    | GltfError Gltf.Error


type alias Model =
    { viewport : XYZMika.XYZ.Viewport
    , viewPortElement : Maybe Browser.Dom.Element
    , keyboard : Keyboard.State
    , dragon : Dragon
    , time : Float
    , paused : Bool
    , asset : Asset
    , selectedTreeIndex : Maybe Int
    , sceneOptions : SceneOptions.Options
    , sceneSize : Float
    , fallbackTexture : RemoteData Error WebGL.Texture.Texture
    , environmentTexture : RemoteData Error WebGL.Texture.Texture
    , specularEnvironmentTexture : RemoteData Error WebGL.Texture.Texture
    , brdfLUTTexture : RemoteData Error WebGL.Texture.Texture
    , scene : RemoteData Error (Scene Scene.ObjectId Material.Name)
    , queryResult : RemoteData Error Gltf.QueryResult
    , animations : List Animation
    , activeAnimation : Maybe Animation
    , activeCamera : Maybe Gltf.Camera.Index
    , gltf : Gltf
    }


init : Asset -> Model
init asset =
    { viewport = XYZMika.XYZ.Viewport 0 0
    , viewPortElement = Nothing
    , time = 0
    , paused = False
    , keyboard = Keyboard.init
    , dragon = Dragon.init
    , asset = asset
    , selectedTreeIndex = Nothing
    , sceneOptions =
        SceneOptions.create
            |> SceneOptions.toggle SceneOptions.showGridYOption
    , sceneSize = 999999999
    , fallbackTexture = RemoteData.Loading
    , environmentTexture = RemoteData.Loading
    , specularEnvironmentTexture = RemoteData.Loading
    , brdfLUTTexture = RemoteData.Loading
    , scene = RemoteData.Loading
    , queryResult = RemoteData.Loading
    , animations = []
    , activeAnimation = Nothing
    , activeCamera = Nothing
    , gltf = Gltf.init
    }


mapSceneOptions : (SceneOptions.Options -> SceneOptions.Options) -> Model -> Model
mapSceneOptions f model =
    { model | sceneOptions = f model.sceneOptions }


viewportFromDomElement : Browser.Dom.Element -> XYZMika.XYZ.Viewport
viewportFromDomElement { element, viewport } =
    let
        aspect =
            viewport.height / viewport.width
    in
    if viewport.width >= viewport.height then
        { width = maxResolution |> round
        , height = maxResolution * aspect |> round
        }

    else
        { width = maxResolution / aspect |> round
        , height = maxResolution |> round
        }


type DragTarget
    = Default
    | CameraOrbit
    | CameraPan
    | CameraZoom


dragTarget : Model -> DragTarget
dragTarget model =
    [ ( Keyboard.isKeyDown Keyboard.Alt model.keyboard && Keyboard.isKeyDown Keyboard.Shift model.keyboard
      , CameraPan
      )
    , ( Keyboard.isKeyDown Keyboard.Alt model.keyboard
      , CameraOrbit
      )
    , ( Keyboard.isKeyDown Keyboard.Shift model.keyboard
      , CameraZoom
      )
    ]
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault Default
