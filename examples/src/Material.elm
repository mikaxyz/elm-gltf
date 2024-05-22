module Material exposing (Name(..), renderer)

import WebGL exposing (Entity)
import XYZMika.XYZ.Material as Material
import XYZMika.XYZ.Material.Advanced
import XYZMika.XYZ.Material.Color
import XYZMika.XYZ.Material.Simple
import XYZMika.XYZ.Material.Skinned
import XYZMika.XYZ.Scene.Object exposing (Object)
import XYZMika.XYZ.Scene.Uniforms exposing (Uniforms)


type Name
    = Simple
    | Color
    | Advanced
    | Skinned


renderer :
    Name
    -> Material.Options
    -> Uniforms u
    -> Object objectId materialId
    -> Entity
renderer name =
    case name of
        Simple ->
            XYZMika.XYZ.Material.Simple.renderer

        Color ->
            XYZMika.XYZ.Material.Color.renderer

        Advanced ->
            XYZMika.XYZ.Material.Advanced.renderer

        Skinned ->
            XYZMika.XYZ.Material.Skinned.renderer
