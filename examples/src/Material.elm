module Material exposing (Name(..), renderer)

import Gltf.Query.ResolvedMaterial
import Page.Example.PbrMaterial
import WebGL exposing (Entity)
import WebGL.Texture
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Color
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Name
    = Simple
    | Color
    | Advanced
    | PbrMaterial Gltf.Query.ResolvedMaterial.Material


renderer :
    WebGL.Texture.Texture
    -> Name
    -> Material.Options
    -> Uniforms u
    -> Object objectId materialId
    -> Entity
renderer fallbackTexture name =
    case name of
        Simple ->
            XYZMika.XYZ.Material.Simple.renderer

        Color ->
            XYZMika.XYZ.Material.Color.renderer

        Advanced ->
            XYZMika.XYZ.Material.Advanced.renderer

        PbrMaterial pbr ->
            Page.Example.PbrMaterial.renderer fallbackTexture pbr
