# Elm glTF library

Import 3d assets from [glTF](https://www.khronos.org/gltf/) (Graphics Library Transmission Format) file format for use in your [Elm WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest) programs.

The aim is to support most of [glTF specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html) but there is still a lot of work to do. See [the example app](https://elm-gltf.mika.xyz/) rendering all [official glTF Sample Assets](https://github.com/KhronosGroup/glTF-Sample-Assets) for an indication what it can/can not do.

## Wiring

This library is designed to load a glTF file (.gltf or .glb) and query its contents. A query may result in multiple Http calls or other effectful things before completing. The package manages these effects and state internally which means you need to do some wiring for it in your application. See the [wiring part in the Gltf module documentation](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#wiring) for instructions.

## Load a file

To load a .gltf file and query for the default scene you use [Gltf.getGltf](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#getGltf):

    Gltf.getGltf url GltfMsg

Alternatively to load scene at index 1:

    Gltf.getGltfWithQuery url (Gltf.sceneQuery 1) GltfMsg

To load a .glb file you would use [Gltf.getBinary](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#getBinary) or [Gltf.getBinaryWithQuery](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#getBinaryWithQuery)

Loading a file always runs a query.

## Queries

Currently you can query for the [default scene](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#defaultSceneQuery) or a [scene by index](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#sceneQuery).

When a query completes your application update function will be called with a [Gltf.QueryResult](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#QueryResult) which you can use to receive [content](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf#content) from the query. The first thing would be to get the node trees in the scene. These nodes you want to store in your model in some form to be rendered in your view. Example:

    GltfOnComplete (Ok queryResult) ->
        ( { model
            | renderables =
                Gltf.nodeTrees queryResult
                    |> List.map (Tree.map toRenderable)
          }
        , Cmd.none
        )

### Scenes
A scene in glTF consists of multiple Node tree data structures. The tree data type is using the [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree#Tree) type from [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest) so your application needs this as a dependency to be able to handle these.

## Rendering

Nodes contain Meshes that contains Materials. Materials may reference textures. To render something you need to make these into [entities](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/WebGL#entity). See the example application for one way to do it. I would like to have examples just using the webgl package and Elm 3d Scene etc. If you make one please ping me and I can link to it.

## Animation

**NOTE:** The animation part of this package is **very much** "work in progress". The way animations are handled is very non performant. There is a lot of lists iterated over every frame. Also the interpolation is hardcoded to `Linear`. Etc.

The module [Gltf.Animation](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf-Animation) exposes functions [animatedProperties](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf-Animation#animatedProperties) and [animatedBoneTransforms](https://package.elm-lang.org/packages/mikaxyz/elm-gltf/3.0.0/Gltf-Animation#animatedBoneTransforms) which gives you transforms for nodes and bones given a timestamp.
