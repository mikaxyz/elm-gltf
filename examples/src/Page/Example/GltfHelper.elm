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

                    25 ->
                        { acc | joint25 = bone.joint, inverseBindMatrix25 = bone.inverseBindMatrix }

                    26 ->
                        { acc | joint26 = bone.joint, inverseBindMatrix26 = bone.inverseBindMatrix }

                    27 ->
                        { acc | joint27 = bone.joint, inverseBindMatrix27 = bone.inverseBindMatrix }

                    28 ->
                        { acc | joint28 = bone.joint, inverseBindMatrix28 = bone.inverseBindMatrix }

                    29 ->
                        { acc | joint29 = bone.joint, inverseBindMatrix29 = bone.inverseBindMatrix }

                    30 ->
                        { acc | joint30 = bone.joint, inverseBindMatrix30 = bone.inverseBindMatrix }

                    31 ->
                        { acc | joint31 = bone.joint, inverseBindMatrix31 = bone.inverseBindMatrix }

                    32 ->
                        { acc | joint32 = bone.joint, inverseBindMatrix32 = bone.inverseBindMatrix }

                    33 ->
                        { acc | joint33 = bone.joint, inverseBindMatrix33 = bone.inverseBindMatrix }

                    34 ->
                        { acc | joint34 = bone.joint, inverseBindMatrix34 = bone.inverseBindMatrix }

                    35 ->
                        { acc | joint35 = bone.joint, inverseBindMatrix35 = bone.inverseBindMatrix }

                    36 ->
                        { acc | joint36 = bone.joint, inverseBindMatrix36 = bone.inverseBindMatrix }

                    37 ->
                        { acc | joint37 = bone.joint, inverseBindMatrix37 = bone.inverseBindMatrix }

                    38 ->
                        { acc | joint38 = bone.joint, inverseBindMatrix38 = bone.inverseBindMatrix }

                    39 ->
                        { acc | joint39 = bone.joint, inverseBindMatrix39 = bone.inverseBindMatrix }

                    40 ->
                        { acc | joint40 = bone.joint, inverseBindMatrix40 = bone.inverseBindMatrix }

                    41 ->
                        { acc | joint41 = bone.joint, inverseBindMatrix41 = bone.inverseBindMatrix }

                    42 ->
                        { acc | joint42 = bone.joint, inverseBindMatrix42 = bone.inverseBindMatrix }

                    43 ->
                        { acc | joint43 = bone.joint, inverseBindMatrix43 = bone.inverseBindMatrix }

                    44 ->
                        { acc | joint44 = bone.joint, inverseBindMatrix44 = bone.inverseBindMatrix }

                    45 ->
                        { acc | joint45 = bone.joint, inverseBindMatrix45 = bone.inverseBindMatrix }

                    46 ->
                        { acc | joint46 = bone.joint, inverseBindMatrix46 = bone.inverseBindMatrix }

                    47 ->
                        { acc | joint47 = bone.joint, inverseBindMatrix47 = bone.inverseBindMatrix }

                    48 ->
                        { acc | joint48 = bone.joint, inverseBindMatrix48 = bone.inverseBindMatrix }

                    49 ->
                        { acc | joint49 = bone.joint, inverseBindMatrix49 = bone.inverseBindMatrix }

                    50 ->
                        { acc | joint50 = bone.joint, inverseBindMatrix50 = bone.inverseBindMatrix }

                    51 ->
                        { acc | joint51 = bone.joint, inverseBindMatrix51 = bone.inverseBindMatrix }

                    52 ->
                        { acc | joint52 = bone.joint, inverseBindMatrix52 = bone.inverseBindMatrix }

                    53 ->
                        { acc | joint53 = bone.joint, inverseBindMatrix53 = bone.inverseBindMatrix }

                    54 ->
                        { acc | joint54 = bone.joint, inverseBindMatrix54 = bone.inverseBindMatrix }

                    55 ->
                        { acc | joint55 = bone.joint, inverseBindMatrix55 = bone.inverseBindMatrix }

                    56 ->
                        { acc | joint56 = bone.joint, inverseBindMatrix56 = bone.inverseBindMatrix }

                    57 ->
                        { acc | joint57 = bone.joint, inverseBindMatrix57 = bone.inverseBindMatrix }

                    58 ->
                        { acc | joint58 = bone.joint, inverseBindMatrix58 = bone.inverseBindMatrix }

                    59 ->
                        { acc | joint59 = bone.joint, inverseBindMatrix59 = bone.inverseBindMatrix }

                    _ ->
                        acc
            )
            Object.boneTransformsIdentity
