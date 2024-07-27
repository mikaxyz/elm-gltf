module Page.Example.GltfHelper exposing
    ( boneTransformsFromAnimations
    , modifiersFromAnimations
    )

import Gltf.Animation exposing (Animation)
import Gltf.NodeIndex exposing (NodeIndex)
import Gltf.Skin exposing (Skin(..))
import Page.Example.Material as Material
import Quaternion exposing (Quaternion)
import XYZMika.XYZ.Scene as Scene
import XYZMika.XYZ.Scene.Object as Object exposing (BoneTransforms, Object)


modifiersFromAnimations : Float -> (NodeIndex -> objectId) -> List Animation -> List (Scene.Modifier objectId Material.Name)
modifiersFromAnimations theta objectIdMap animations =
    let
        toModifier : Gltf.Animation.AnimatedProperty -> Scene.Modifier objectId Material.Name
        toModifier animated =
            case animated of
                Gltf.Animation.AnimatedPositionProperty nodeIndex position ->
                    Scene.ObjectModifier (objectIdMap nodeIndex) (Object.withPosition position)

                Gltf.Animation.AnimatedRotationProperty nodeIndex rotation ->
                    Scene.ObjectModifier (objectIdMap nodeIndex) (Object.withRotation (Quaternion.toMat4 rotation))
    in
    Gltf.Animation.animatedProperties theta animations
        |> List.map toModifier


boneTransformsFromAnimations : Float -> List Animation -> Skin -> BoneTransforms
boneTransformsFromAnimations theta animations skin =
    Gltf.Animation.animatedBoneTransforms theta animations skin
        |> List.foldl
            (\(Gltf.Animation.AnimatedBone bone) acc ->
                case bone.skinIndex of
                    0 ->
                        { acc | joint0 = bone.joint, inverseBindMatrix0 = bone.inverseBindMatrix }

                    1 ->
                        { acc | joint1 = bone.joint, inverseBindMatrix1 = bone.inverseBindMatrix }

                    2 ->
                        { acc | joint2 = bone.joint, inverseBindMatrix2 = bone.inverseBindMatrix }

                    3 ->
                        { acc | joint3 = bone.joint, inverseBindMatrix3 = bone.inverseBindMatrix }

                    4 ->
                        { acc | joint4 = bone.joint, inverseBindMatrix4 = bone.inverseBindMatrix }

                    5 ->
                        { acc | joint5 = bone.joint, inverseBindMatrix5 = bone.inverseBindMatrix }

                    6 ->
                        { acc | joint6 = bone.joint, inverseBindMatrix6 = bone.inverseBindMatrix }

                    7 ->
                        { acc | joint7 = bone.joint, inverseBindMatrix7 = bone.inverseBindMatrix }

                    8 ->
                        { acc | joint8 = bone.joint, inverseBindMatrix8 = bone.inverseBindMatrix }

                    9 ->
                        { acc | joint9 = bone.joint, inverseBindMatrix9 = bone.inverseBindMatrix }

                    10 ->
                        { acc | joint10 = bone.joint, inverseBindMatrix10 = bone.inverseBindMatrix }

                    11 ->
                        { acc | joint11 = bone.joint, inverseBindMatrix11 = bone.inverseBindMatrix }

                    12 ->
                        { acc | joint12 = bone.joint, inverseBindMatrix12 = bone.inverseBindMatrix }

                    13 ->
                        { acc | joint13 = bone.joint, inverseBindMatrix13 = bone.inverseBindMatrix }

                    14 ->
                        { acc | joint14 = bone.joint, inverseBindMatrix14 = bone.inverseBindMatrix }

                    15 ->
                        { acc | joint15 = bone.joint, inverseBindMatrix15 = bone.inverseBindMatrix }

                    16 ->
                        { acc | joint16 = bone.joint, inverseBindMatrix16 = bone.inverseBindMatrix }

                    17 ->
                        { acc | joint17 = bone.joint, inverseBindMatrix17 = bone.inverseBindMatrix }

                    18 ->
                        { acc | joint18 = bone.joint, inverseBindMatrix18 = bone.inverseBindMatrix }

                    19 ->
                        { acc | joint19 = bone.joint, inverseBindMatrix19 = bone.inverseBindMatrix }

                    20 ->
                        { acc | joint20 = bone.joint, inverseBindMatrix20 = bone.inverseBindMatrix }

                    21 ->
                        { acc | joint21 = bone.joint, inverseBindMatrix21 = bone.inverseBindMatrix }

                    22 ->
                        { acc | joint22 = bone.joint, inverseBindMatrix22 = bone.inverseBindMatrix }

                    23 ->
                        { acc | joint23 = bone.joint, inverseBindMatrix23 = bone.inverseBindMatrix }

                    24 ->
                        { acc | joint24 = bone.joint, inverseBindMatrix24 = bone.inverseBindMatrix }

                    _ ->
                        acc
            )
            Object.boneTransformsIdentity
