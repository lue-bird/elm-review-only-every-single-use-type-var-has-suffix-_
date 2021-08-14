module SyntaxHelp exposing (allTypeVarsInType, collectTypeVarsFromDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))


collectTypeVarsFromDeclaration : Declaration -> List (Node String)
collectTypeVarsFromDeclaration declaration =
    case declaration of
        FunctionDeclaration { signature } ->
            case signature of
                Nothing ->
                    []

                Just (Node _ { typeAnnotation }) ->
                    typeAnnotation |> allTypeVarsInType

        PortDeclaration { typeAnnotation } ->
            allTypeVarsInType typeAnnotation

        CustomTypeDeclaration _ ->
            []

        AliasDeclaration _ ->
            []

        Destructuring _ _ ->
            []

        InfixDeclaration _ ->
            []


allTypeVarsInType : Node TypeAnnotation -> List (Node String)
allTypeVarsInType type_ =
    case Node.value type_ of
        Unit ->
            []

        GenericType typeVarName ->
            [ Node (Node.range type_) typeVarName ]

        Typed _ typeArguments ->
            typeArguments
                |> List.concatMap allTypeVarsInType

        FunctionTypeAnnotation a b ->
            [ a, b ]
                |> List.concatMap allTypeVarsInType

        Tupled innerTypes ->
            innerTypes
                |> List.concatMap allTypeVarsInType

        Record fields ->
            fields
                |> List.map (\(Node _ ( _, type__ )) -> type__)
                |> List.concatMap allTypeVarsInType

        GenericRecord toExtend fields ->
            toExtend
                :: (fields
                        |> Node.value
                        |> List.map (\(Node _ ( _, type__ )) -> type__)
                        |> List.concatMap allTypeVarsInType
                   )
