module Page.Example.Scene exposing
    ( Config
    , ObjectId(..)
    , init
    , modifiers
    )

import Color
import Gltf exposing (Gltf)
import Gltf.Query as Query
import Gltf.Query.TriangularMesh as TriangularMesh exposing (TriangularMesh(..))
import Internal.Node as Node exposing (Index(..), Node(..))
import Internal.Scene
import Material
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Page.Example.GltfHelper as GltfHelper
import Quaternion
import Tree exposing (Tree)
import XYZMika.XYZ.Data.Vertex as Vertex exposing (Vertex)
import XYZMika.XYZ.Mesh.Primitives as Primitives
import XYZMika.XYZ.Scene as Scene exposing (Scene)
import XYZMika.XYZ.Scene.Camera as Camera exposing (Camera)
import XYZMika.XYZ.Scene.Graph as Graph exposing (Graph)
import XYZMika.XYZ.Scene.Light as Light
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)


type ObjectId
    = Mesh Node.Index
    | SkinnedMesh
    | Bone Node.Index


type alias Config =
    { camera : Camera
    , projection : { fov : Float, near : Float, far : Float }
    }


init : Gltf -> (ObjectId -> objectId) -> Config -> Scene objectId Material.Name
init gltf objectIdMap config =
    let
        nodes : List (Tree (Object objectId Material.Name))
        nodes =
            gltf
                |> Query.sceneNodeTrees (Internal.Scene.Index 0)
                |> Result.withDefault []
                |> List.map (Tree.map (Query.nodeFromNode gltf >> objectFromNode objectIdMap))

        { cameraTarget, cameraPosition, cameraProjection } =
            frameScene config nodes
    in
    lights
        :: nodes
        |> Graph.graph (Object.group "")
        |> Scene.init
        |> Scene.withCameraMap (always config.camera)
        |> Scene.withCameraMap (Camera.withTarget cameraTarget >> Camera.withPosition cameraPosition)
        |> Scene.withPerspectiveProjection cameraProjection


frameScene :
    Config
    -> List (Tree (Object objectId Material.Name))
    ->
        { cameraTarget : Vec3
        , cameraPosition : Vec3
        , cameraProjection : { fov : Float, near : Float, far : Float }
        }
frameScene config nodes =
    let
        bounds : { min : Vec3, max : Vec3 }
        bounds =
            nodes
                |> List.concatMap Tree.flatten
                |> List.map Object.boundingBox
                |> List.foldl
                    (\( v1, v2 ) ( bMin, bMax ) ->
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
    }


objectFromNode : (ObjectId -> objectId) -> Query.Node -> Object objectId Material.Name
objectFromNode objectIdMap thing =
    case thing of
        Query.EmptyNode (Query.Properties properties) ->
            Object.group "EMPTY"
                |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                |> (properties.translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                |> (properties.rotation
                        |> Maybe.map (Quaternion.toMat4 >> Object.withRotation)
                        |> Maybe.withDefault identity
                   )

        Query.CameraNode (Query.Properties properties) ->
            Object.group "CAMERA"
                |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                |> (properties.translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                |> (properties.rotation
                        |> Maybe.map (Quaternion.toMat4 >> Object.withRotation)
                        |> Maybe.withDefault identity
                   )

        Query.MeshNode (mesh :: _) (Query.Properties properties) ->
            mesh
                |> objectFromMesh (objectIdMap (Mesh properties.nodeIndex))
                |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                |> (properties.translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                |> (properties.rotation
                        |> Maybe.map (Quaternion.toMat4 >> Object.withRotation)
                        |> Maybe.withDefault identity
                   )
                |> Object.withColor Color.gray
                |> Object.withMaterialName Material.Advanced

        Query.SkinnedMeshNode (mesh :: _) skin (Query.Properties properties) ->
            let
                obj =
                    mesh
                        |> objectFromMesh (objectIdMap SkinnedMesh)
                        |> GltfHelper.objectWithSkin skin
                        |> (properties.nodeName |> Maybe.map Object.withName |> Maybe.withDefault identity)
                        |> (properties.translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                        |> (properties.rotation
                                |> Maybe.map (Quaternion.toMat4 >> Object.withRotation)
                                |> Maybe.withDefault identity
                           )
                        |> Object.withColor Color.green
                        |> Object.withMaterialName Material.Skinned
            in
            obj

        Query.MeshNode [] (Query.Properties properties) ->
            let
                name (Node.Index nodeIndex) =
                    Maybe.withDefault "Node" properties.nodeName ++ "(" ++ String.fromInt nodeIndex ++ ")"
            in
            Primitives.bone3 0.1
                |> Object.objectWithTriangles (objectIdMap (Bone properties.nodeIndex))
                |> Object.withName (name properties.nodeIndex)
                |> (properties.translation |> Maybe.map Object.withPosition |> Maybe.withDefault identity)
                |> (properties.rotation
                        |> Maybe.map (Quaternion.toMat4 >> Object.withRotation)
                        |> Maybe.withDefault identity
                   )
                |> Object.withColor Color.gray
                |> Object.disable
                |> Object.withMaterialName Material.Advanced

        Query.SkinnedMeshNode [] _ (Query.Properties _) ->
            Object.group ""


objectFromMesh : objectId -> TriangularMesh -> Object objectId Material.Name
objectFromMesh objectId triangularMesh =
    case triangularMesh of
        TriangularMesh vertices ->
            vertices
                |> List.map (\( v1, v2, v3 ) -> ( toVertex v1, toVertex v2, toVertex v3 ))
                |> Object.objectWithTriangles objectId

        IndexedTriangularMesh mesh ->
            mesh
                |> Tuple.mapFirst (List.map toVertex)
                |> Object.objectObjectWithIndexedTriangles objectId


modifiers : Float -> Gltf -> List (Scene.Modifier ObjectId Material.Name)
modifiers theta gltf =
    boneDeformer theta SkinnedMesh gltf
        :: GltfHelper.modifiers theta Mesh gltf
        ++ GltfHelper.modifiers theta Bone gltf


boneDeformer : Float -> objectId -> Gltf -> Scene.Modifier objectId Material.Name
boneDeformer theta objectId gltf =
    Scene.ObjectModifier objectId (boneDeformerF theta gltf)


boneDeformerF : Float -> Gltf -> Object objectId Material.Name -> Object objectId Material.Name
boneDeformerF theta gltf obj =
    obj
        |> Object.skin
        |> Maybe.map
            (\skin ->
                let
                    skeleton : Maybe (Tree Node)
                    skeleton =
                        skin.joints
                            |> List.head
                            |> Maybe.andThen
                                (\rootNode ->
                                    Query.nodeTree (Node.Index rootNode) gltf
                                        |> Result.toMaybe
                                )

                    boneTransforms : BoneTransforms
                    boneTransforms =
                        skeleton
                            |> Maybe.map (GltfHelper.boneTransformsFromFirstAnimation theta gltf skin)
                            |> Maybe.withDefault Object.boneTransformsIdentity
                in
                obj |> Object.withBoneTransforms boneTransforms
            )
        |> Maybe.withDefault obj


toVertex : TriangularMesh.Vertex -> Vertex
toVertex v =
    v.position
        |> Vertex.vertex
        |> (v.normal
                |> Maybe.map Vertex.withNormal
                |> Maybe.withDefault identity
           )
        -- TODO: Time to remove concrete Vertex type?
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


lights : Graph (Object objectId Material.Name)
lights =
    Graph.shallow (Object.group "LIGHTS")
        [ pointLight 3.0 (Vec3.vec3 1 2 2) (vec3 0.2 0.15 0.1)
        , pointLight 2.0 (Vec3.vec3 -2 3 1) (vec3 0.3 0.25 0.1)
        , pointLight 2.0 (Vec3.vec3 2 1 0) (vec3 0.4 0.1 0.05)
        , pointLight 5.0 (Vec3.vec3 1 4 -2) (vec3 0.1 0.1 0.15)
        ]


pointLight : Float -> Vec3 -> Vec3 -> Object objectId materialId
pointLight intensity position color =
    Object.light
        (Light.pointLight position
            |> Light.withIntensity intensity
            |> Light.withColorVec color
        )
