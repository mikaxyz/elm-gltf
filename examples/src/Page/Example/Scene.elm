module Page.Example.Scene exposing
    ( Config
    , ObjectId(..)
    , graphFromNodes
    , initWithNodes
    , modifiers
    )

import Color
import Gltf.Animation exposing (Animation)
import Gltf.Camera
import Gltf.Material
import Gltf.Mesh exposing (Mesh(..))
import Gltf.Node
import Gltf.NodeIndex exposing (NodeIndex(..))
import Gltf.Skin exposing (Skin)
import Gltf.Transform exposing (Transform)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4
import Page.Example.GltfHelper as GltfHelper
import Page.Example.Material as Material
import Quaternion
import Tree exposing (Tree)
import WebGL.Settings.DepthTest
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Gizmo
import XYZMika.XYZ.Mesh.Primitives as Primitives
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)


type ObjectId
    = Empty NodeIndex
    | Mesh NodeIndex
    | SkinnedMesh Gltf.Skin.Index
    | Bone NodeIndex
    | Camera Gltf.Camera.Index NodeIndex


type alias Config =
    { camera : Camera
    , projection : { fov : Float, near : Float, far : Float }
    , sceneSize : Float
    }


initWithNodes : List (Tree Gltf.Node.Node) -> Config -> Scene ObjectId Material.Name
initWithNodes nodes config =
    let
        { cameraTarget, cameraPosition, cameraProjection, bounds } =
            frameScene config nodes
    in
    graphFromNodes nodes config
        |> Scene.init
        |> Scene.withCameraMap (always config.camera)
        |> Scene.withCameraMap (Camera.withTarget cameraTarget >> Camera.withPosition cameraPosition)
        |> Scene.withPerspectiveProjection cameraProjection


graphFromNodes : List (Tree Gltf.Node.Node) -> Config -> Graph (Object ObjectId Material.Name)
graphFromNodes nodeTrees config =
    let
        nodesToObjects : Tree Gltf.Node.Node -> Tree (Object ObjectId Material.Name)
        nodesToObjects nodes =
            nodes
                |> Tree.map objectsFromNode
                |> Tree.restructure identity
                    (\( mesh, childMeshes ) c ->
                        Tree.tree mesh (childMeshes |> List.map Tree.singleton |> List.append c)
                    )

        objects : List (Tree (Object ObjectId Material.Name))
        objects =
            nodeTrees |> List.map nodesToObjects
    in
    lights config.sceneSize
        :: objects
        |> Graph.graph (Object.group "")


frameScene :
    Config
    -> List (Tree Gltf.Node.Node)
    ->
        { cameraTarget : Vec3
        , cameraPosition : Vec3
        , cameraProjection : { fov : Float, near : Float, far : Float }
        , bounds : { min : Vec3, max : Vec3 }
        }
frameScene config nodes =
    let
        getBounds : List Gltf.Mesh.Vertex -> ( Vec3, Vec3 )
        getBounds vertices =
            vertices
                |> List.map .position
                |> List.foldl
                    (\v ( min, max ) ->
                        ( vMin min v, vMax max v )
                    )
                    ( vec3 0 0 0, vec3 0 0 0 )

        vMin : Vec3 -> Vec3 -> Vec3
        vMin v1 v2 =
            vec3
                (min (Vec3.getX v1) (Vec3.getX v2))
                (min (Vec3.getY v1) (Vec3.getY v2))
                (min (Vec3.getZ v1) (Vec3.getZ v2))

        vMax : Vec3 -> Vec3 -> Vec3
        vMax v1 v2 =
            vec3
                (max (Vec3.getX v1) (Vec3.getX v2))
                (max (Vec3.getY v1) (Vec3.getY v2))
                (max (Vec3.getZ v1) (Vec3.getZ v2))

        joinBoundingBoxes : List ( Vec3, Vec3 ) -> ( Vec3, Vec3 )
        joinBoundingBoxes boundingBoxes =
            boundingBoxes
                |> List.foldl
                    (\( min, max ) ( aMin, aMax ) ->
                        ( vMin min aMin, vMax max aMax )
                    )
                    ( vec3 0 0 0, vec3 0 0 0 )

        triangularMeshToBounds : Mesh -> ( Vec3, Vec3 )
        triangularMeshToBounds mesh =
            case mesh of
                Points _ vertices ->
                    vertices |> getBounds

                Lines _ vertices ->
                    vertices |> List.concatMap (\( v1, v2 ) -> [ v1, v2 ]) |> getBounds

                LineLoop _ vertices ->
                    vertices |> getBounds

                LineStrip _ vertices ->
                    vertices |> getBounds

                TriangleStrip _ vertices ->
                    vertices |> getBounds

                TriangleFan _ vertices ->
                    vertices |> getBounds

                TriangularMesh _ vertices ->
                    vertices |> List.concatMap (\( v1, v2, v3 ) -> [ v1, v2, v3 ]) |> getBounds

                IndexedTriangularMesh _ ( vertices, _ ) ->
                    vertices |> getBounds

        triangularMeshesToBounds : List Mesh -> ( Vec3, Vec3 )
        triangularMeshesToBounds meshes =
            meshes
                |> List.map triangularMeshToBounds
                |> joinBoundingBoxes

        nodeTransformAndBounds : Gltf.Node.Node -> ( Mat4, Maybe ( Vec3, Vec3 ) )
        nodeTransformAndBounds node =
            let
                transformToMat : Transform -> Mat4
                transformToMat transform =
                    case transform of
                        Gltf.Transform.TRS { translation, rotation, scale } ->
                            let
                                r =
                                    rotation |> Maybe.map Quaternion.toMat4 |> Maybe.withDefault Mat4.identity

                                s =
                                    scale |> Maybe.withDefault (vec3 1 1 1) |> Mat4.makeScale
                            in
                            translation
                                |> Maybe.withDefault (vec3 0 0 0)
                                |> Mat4.makeTranslate
                                |> Mat4.mul r
                                |> Mat4.mul s

                        Gltf.Transform.Matrix mat ->
                            mat
            in
            case node of
                Gltf.Node.Camera _ (Gltf.Node.Properties properties) ->
                    ( transformToMat properties.transform, Nothing )

                Gltf.Node.Empty (Gltf.Node.Properties properties) ->
                    ( transformToMat properties.transform, Nothing )

                Gltf.Node.Mesh meshes (Gltf.Node.Properties properties) ->
                    ( transformToMat properties.transform, Just (triangularMeshesToBounds meshes) )

                Gltf.Node.SkinnedMesh meshes _ (Gltf.Node.Properties properties) ->
                    ( transformToMat properties.transform, Just (triangularMeshesToBounds meshes) )

                Gltf.Node.Bone _ _ (Gltf.Node.Properties properties) ->
                    ( transformToMat properties.transform, Nothing )

        treeWithGlobalMatrix : Mat4 -> Tree ( Mat4, Maybe ( Vec3, Vec3 ) ) -> Tree ( Mat4, Maybe ( Vec3, Vec3 ) )
        treeWithGlobalMatrix globalMat tree =
            let
                mat : Mat4
                mat =
                    Tree.label tree |> Tuple.first |> Mat4.mul globalMat
            in
            tree
                |> Tree.mapLabel (\( _, bb ) -> ( mat, bb |> Maybe.map (\( bb1, bb2 ) -> ( Mat4.transform mat bb1, Mat4.transform mat bb2 )) ))
                |> Tree.mapChildren (List.map (treeWithGlobalMatrix mat))

        bounds : { min : Vec3, max : Vec3 }
        bounds =
            nodes
                |> List.map (Tree.map nodeTransformAndBounds)
                |> List.map (treeWithGlobalMatrix Mat4.identity >> Tree.map Tuple.second)
                |> List.concatMap Tree.flatten
                |> List.foldl
                    (\bb ( bMin, bMax ) ->
                        case bb of
                            Just ( v1, v2 ) ->
                                let
                                    ( x1, x2 ) =
                                        ( Vec3.getX v1, Vec3.getX v2 )

                                    ( y1, y2 ) =
                                        ( Vec3.getY v1, Vec3.getY v2 )

                                    ( z1, z2 ) =
                                        ( Vec3.getZ v1, Vec3.getZ v2 )

                                    ( xMin, xMax ) =
                                        ( min x1 x2, max x1 x2 )

                                    ( yMin, yMax ) =
                                        ( min y1 y2, max y1 y2 )

                                    ( zMin, zMax ) =
                                        ( min z1 z2, max z1 z2 )
                                in
                                ( { bMin | x = min bMin.x xMin, y = min bMin.y yMin, z = min bMin.z zMin }
                                , { bMax | x = max bMax.x xMax, y = max bMax.y yMax, z = max bMax.z zMax }
                                )

                            Nothing ->
                                ( bMin, bMax )
                    )
                    ( Vec3.toRecord (vec3 0 0 0), Vec3.toRecord (vec3 0 0 0) )
                |> (\( min, max ) -> { min = Vec3.fromRecord min, max = Vec3.fromRecord max })

        center : Vec3
        center =
            let
                ab =
                    Vec3.direction bounds.max bounds.min

                d =
                    Vec3.distance bounds.min bounds.max
            in
            bounds.min
                |> Vec3.add (Vec3.scale (0.5 * d) ab)

        boundSphereRadius : Float
        boundSphereRadius =
            [ bounds.min, bounds.max ]
                |> List.map (Vec3.distance center)
                |> List.maximum
                |> Maybe.withDefault 1.0

        fov : Float
        fov =
            config.projection.fov |> degrees

        camDistance : Float
        camDistance =
            (boundSphereRadius * 2.0) / tan (fov / 2.0)

        cameraPosition : Vec3
        cameraPosition =
            Camera.position config.camera |> Vec3.normalize |> Vec3.scale camDistance

        projection : { fov : Float, near : Float, far : Float }
        projection =
            config.projection

        camFocalDistance : Float
        camFocalDistance =
            Vec3.distance center cameraPosition
    in
    { cameraTarget = center
    , cameraPosition = cameraPosition
    , cameraProjection =
        { projection | near = 0.01 * (camFocalDistance - boundSphereRadius), far = 100 * (camFocalDistance - boundSphereRadius) }
    , bounds = bounds
    }


objectIdFromNode : Gltf.Node.Node -> ObjectId
objectIdFromNode node =
    case node of
        Gltf.Node.Empty (Gltf.Node.Properties properties) ->
            Empty properties.nodeIndex

        Gltf.Node.Camera cameraId (Gltf.Node.Properties properties) ->
            Camera cameraId properties.nodeIndex

        Gltf.Node.Mesh _ (Gltf.Node.Properties properties) ->
            Mesh properties.nodeIndex

        Gltf.Node.SkinnedMesh _ skinIndex _ ->
            SkinnedMesh skinIndex

        Gltf.Node.Bone _ _ (Gltf.Node.Properties properties) ->
            Bone properties.nodeIndex


objectsFromNode : Gltf.Node.Node -> ( Object ObjectId Material.Name, List (Object ObjectId Material.Name) )
objectsFromNode node =
    let
        applyTransform : Transform -> Object id materialId -> Object id materialId
        applyTransform transform object =
            case transform of
                Gltf.Transform.TRS { translation, rotation, scale } ->
                    object
                        |> (translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                        |> (\object_ ->
                                let
                                    r : Mat4
                                    r =
                                        rotation |> Maybe.map Quaternion.toMat4 |> Maybe.withDefault Mat4.identity

                                    s : Mat4
                                    s =
                                        -- No scale on object
                                        scale |> Maybe.withDefault (vec3 1 1 1) |> Mat4.makeScale
                                in
                                object_ |> Object.withRotation (Mat4.mulAffine s r)
                           )

                Gltf.Transform.Matrix mat ->
                    Object.withRotation mat object

        objectId : ObjectId
        objectId =
            objectIdFromNode node
    in
    case node of
        Gltf.Node.Empty (Gltf.Node.Properties properties) ->
            ( Object.groupWithId objectId "EMPTY"
                |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                |> applyTransform properties.transform
            , []
            )

        Gltf.Node.Camera cameraId (Gltf.Node.Properties properties) ->
            ( Object.groupWithId objectId "CAMERA"
                |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                |> applyTransform properties.transform
            , []
            )

        Gltf.Node.Mesh meshes (Gltf.Node.Properties properties) ->
            case meshes of
                mesh :: rest ->
                    ( mesh
                        |> objectFromMesh objectId
                        |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                        |> applyTransform properties.transform
                    , rest |> List.map (objectFromMesh (Mesh properties.nodeIndex))
                    )

                [] ->
                    ( Object.group "" |> applyTransform properties.transform
                    , []
                    )

        Gltf.Node.SkinnedMesh meshes skinIndex (Gltf.Node.Properties properties) ->
            case meshes of
                mesh :: rest ->
                    ( mesh
                        |> objectFromMesh objectId
                        |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                        |> applyTransform properties.transform
                    , rest |> List.map (objectFromMesh (SkinnedMesh skinIndex))
                    )

                [] ->
                    ( Object.group "" |> applyTransform properties.transform
                    , []
                    )

        Gltf.Node.Bone _ maybeLength (Gltf.Node.Properties properties) ->
            let
                name (NodeIndex nodeIndex) =
                    Maybe.withDefault "Node" properties.nodeName ++ "(" ++ String.fromInt nodeIndex ++ ")"

                geometry =
                    case maybeLength of
                        Just length ->
                            Primitives.bone3 length
                                |> Object.objectWithTriangles objectId

                        Nothing ->
                            XYZMika.XYZ.Mesh.Gizmo.axis
                                |> Object.objectWithTriangles objectId
            in
            ( geometry
                |> Object.withName (name properties.nodeIndex)
                |> applyTransform properties.transform
                |> Object.withColor Color.gray
                |> Object.withGlSetting
                    (WebGL.Settings.DepthTest.always
                        { write = True
                        , near = 0
                        , far = 1
                        }
                    )
                |> Object.disable
            , []
            )


objectFromMesh : objectId -> Mesh -> Object objectId Material.Name
objectFromMesh objectId mesh =
    let
        withMaterial : Maybe Gltf.Material.Material -> Object id Material.Name -> Object id Material.Name
        withMaterial maybeMaterial =
            case maybeMaterial of
                Just material ->
                    Object.withMaterialName (Material.PbrMaterial material)

                Nothing ->
                    Object.withMaterialName Material.Default
    in
    case mesh of
        Points material vertices ->
            vertices
                |> List.map toVertex
                |> Object.initWithPoints
                |> withMaterial material

        Lines material vertices ->
            vertices
                |> List.map (Tuple.mapBoth toVertex toVertex)
                |> Object.initWithLines
                |> withMaterial material

        LineLoop material vertices ->
            vertices
                |> List.map toVertex
                |> Object.initWithLineLoop
                |> withMaterial material

        LineStrip material vertices ->
            vertices
                |> List.map toVertex
                |> Object.initWithLineStrip
                |> withMaterial material

        TriangleStrip material vertices ->
            vertices
                |> List.map toVertex
                |> Object.initWithTriangleStrip
                |> withMaterial material

        TriangleFan material vertices ->
            vertices
                |> List.map toVertex
                |> Object.initWithTriangleFan
                |> withMaterial material

        TriangularMesh material vertices ->
            vertices
                |> List.map (\( v1, v2, v3 ) -> ( toVertex v1, toVertex v2, toVertex v3 ))
                |> Object.objectWithTriangles objectId
                |> withMaterial material

        IndexedTriangularMesh material vertices ->
            vertices
                |> Tuple.mapFirst (List.map toVertex)
                |> Object.objectObjectWithIndexedTriangles objectId
                |> withMaterial material


modifiers : Float -> Maybe Animation -> List Skin -> List (Scene.Modifier ObjectId Material.Name)
modifiers theta maybeAnimation skins =
    case maybeAnimation of
        Just animation ->
            boneTransformModifiers theta SkinnedMesh [ animation ] skins
                ++ GltfHelper.modifiersFromAnimations theta Empty [ animation ]
                ++ GltfHelper.modifiersFromAnimations theta Mesh [ animation ]
                ++ GltfHelper.modifiersFromAnimations theta Bone [ animation ]

        Nothing ->
            []


boneTransformModifiers : Float -> (Gltf.Skin.Index -> objectId) -> List Animation -> List Skin -> List (Scene.Modifier objectId Material.Name)
boneTransformModifiers theta objectId animations skins =
    skins
        |> List.map
            (\(Gltf.Skin.Skin skin) ->
                let
                    boneTransforms : BoneTransforms
                    boneTransforms =
                        GltfHelper.boneTransformsFromAnimations theta animations (Gltf.Skin.Skin skin)
                in
                Scene.ObjectModifier (objectId skin.index) (Object.withBoneTransforms boneTransforms)
            )


toVertex : Gltf.Mesh.Vertex -> Vertex
toVertex v =
    v.position
        |> Vertex.vertex
        |> (v.texCoords
                |> Maybe.map Vertex.withUV
                |> Maybe.withDefault identity
           )
        |> (v.normal
                |> Maybe.map Vertex.withNormal
                |> Maybe.withDefault identity
           )
        |> (v.tangent
                |> Maybe.map
                    (\tangent ->
                        let
                            { x, y, z } =
                                Vec4.toRecord tangent
                        in
                        vec3 x y z |> Vertex.withTangent
                    )
                |> Maybe.withDefault identity
           )
        |> (v.color
                |> Maybe.map
                    (\color ->
                        let
                            { x, y, z } =
                                Vec4.toRecord color
                        in
                        vec3 x y z |> Vertex.withColor
                    )
                |> Maybe.withDefault identity
           )
        |> (v.weights
                |> Maybe.map Vertex.withWeights
                |> Maybe.withDefault identity
           )
        |> (v.joints
                |> Maybe.map
                    (\{ j1, j2, j3, j4 } ->
                        Vertex.withJoints j1 j2 j3 j4
                    )
                |> Maybe.withDefault identity
           )


lights : Float -> Graph (Object objectId Material.Name)
lights d =
    Graph.shallow (Object.group "LIGHTS")
        [ pointLight 0.8 (Vec3.vec3 1 2 2 |> Vec3.scale (max 1 d)) (vec3 0.9 0.7 0.6)
        , pointLight 0.3 (Vec3.vec3 -2 3 1 |> Vec3.scale (max 1 d)) (vec3 0.7 0.9 0.9)
        , pointLight 0.3 (Vec3.vec3 2 1 0 |> Vec3.scale (max 1 d)) (vec3 0.9 0.5 0.5)
        , pointLight 0.4 (Vec3.vec3 1 4 -2 |> Vec3.scale (max 1 d)) (vec3 0.7 0.7 0.9)
        , pointLight 0.5 (Vec3.vec3 -2 -1 -3 |> Vec3.scale (max 1 d)) (vec3 0.6 0.8 0.9)
        ]


pointLight : Float -> Vec3 -> Vec3 -> Object objectId materialId
pointLight intensity position color =
    Object.light
        (Light.pointLight position
            |> Light.withIntensity intensity
            |> Light.withColorVec color
        )
