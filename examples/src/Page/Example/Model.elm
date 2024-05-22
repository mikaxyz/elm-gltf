module Page.Example.Model exposing
    ( Asset(..)
    , DragTarget(..)
    , Model
    , Msg(..)
    , dragTarget
    , init
    , mapSceneOptions
    , viewportFromDomElement
    )

import Browser.Dom
import Http
import Keyboard
import Material
import Page.Example.Scene as Scene
import RemoteData exposing (RemoteData)
import SampleAssets
import XYZMika.Dragon as Dragon exposing (Dragon)
import XYZMika.XYZ
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Options as SceneOptions
import Xyz.Gltf.Raw.Gltf exposing (Gltf)


maxResolution : number
maxResolution =
    1200


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
    | GltfReceived (Result Http.Error Gltf)
    | OnViewportElement (Result Browser.Dom.Error Browser.Dom.Element)


type Asset
    = Local String
    | SampleAsset SampleAssets.Asset


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
    , gltf : RemoteData Http.Error Gltf
    , scene : RemoteData Http.Error (Scene Scene.ObjectId Material.Name)
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
    , sceneOptions = SceneOptions.create |> SceneOptions.toggle SceneOptions.showGridYOption
    , gltf = RemoteData.Loading
    , scene = RemoteData.Loading
    }


mapSceneOptions : (SceneOptions.Options -> SceneOptions.Options) -> Model -> Model
mapSceneOptions f model =
    { model | sceneOptions = f model.sceneOptions }


viewportFromDomElement : Browser.Dom.Element -> XYZMika.XYZ.Viewport
viewportFromDomElement { element } =
    let
        aspect =
            element.height / element.width
    in
    if element.width >= element.height then
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
