module SyntaxHelp exposing (collectLetDeclarationsFromExpression, collectTypeVarsFromDeclaration, collectTypeVarsFromType, collectTypesFromLetDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List.Extra as List
import List.NonEmpty


collectTypeVarsFromDeclaration : Node Declaration -> List (Node String)
collectTypeVarsFromDeclaration declaration =
    case Node.value declaration of
        FunctionDeclaration { signature } ->
            case signature of
                Nothing ->
                    []

                Just type_ ->
                    (type_ |> Node.value |> .typeAnnotation)
                        |> collectTypeVarsFromType

        CustomTypeDeclaration { generics, constructors } ->
            generics
                ++ (constructors
                        |> List.map (Node.value >> .arguments)
                        |> List.concatMap
                            (List.concatMap collectTypeVarsFromType)
                   )

        PortDeclaration { typeAnnotation } ->
            collectTypeVarsFromType typeAnnotation

        AliasDeclaration _ ->
            []

        Destructuring _ _ ->
            []

        InfixDeclaration _ ->
            []


collectTypeVarsFromType : Node TypeAnnotation -> List (Node String)
collectTypeVarsFromType type_ =
    let
        typeVarIfEndsWith_ typeVarName =
            Node (Node.range type_) typeVarName
    in
    case Node.value type_ of
        Unit ->
            []

        GenericType typeVarName ->
            [ typeVarIfEndsWith_ typeVarName ]

        Typed _ typeArguments ->
            typeArguments
                |> List.map collectTypeVarsFromType
                |> List.concat

        FunctionTypeAnnotation a b ->
            [ a, b ]
                |> List.map collectTypeVarsFromType
                |> List.concat

        Tupled innerTypes ->
            innerTypes
                |> List.map collectTypeVarsFromType
                |> List.concat

        Record fields ->
            fields
                |> List.map Node.value
                |> List.map (\( _, type__ ) -> type__)
                |> List.map collectTypeVarsFromType
                |> List.concat

        GenericRecord toExtend fields ->
            toExtend
                :: (fields
                        |> Node.value
                        |> List.map Node.value
                        |> List.map (\( _, type__ ) -> type__)
                        |> List.map collectTypeVarsFromType
                        |> List.concat
                   )


collectTypesFromLetDeclaration : Node LetDeclaration -> List (Node TypeAnnotation)
collectTypesFromLetDeclaration letDeclaration =
    case Node.value letDeclaration of
        LetFunction { signature } ->
            case signature of
                Nothing ->
                    []

                Just type_ ->
                    [ type_ |> Node.value |> .typeAnnotation ]

        LetDestructuring _ expr ->
            expr
                |> collectLetDeclarationsFromExpression
                |> List.concatMap collectTypesFromLetDeclaration


collectLetDeclarationsFromExpression : Node Expression -> List (Node LetDeclaration)
collectLetDeclarationsFromExpression expression =
    let
        collectRecordSetterExpressions =
            List.map Node.value
                >> List.map (\( _, expr ) -> expr)
                >> List.concatMap collectLetDeclarationsFromExpression
    in
    case Node.value expression of
        LetExpression letBlock ->
            (letBlock.expression
                |> collectLetDeclarationsFromExpression
            )
                ++ letBlock.declarations

        ListExpr expressions ->
            expressions |> List.concatMap collectLetDeclarationsFromExpression

        TupledExpression expressions ->
            expressions |> List.concatMap collectLetDeclarationsFromExpression

        RecordExpr setters ->
            setters |> collectRecordSetterExpressions

        RecordUpdateExpression _ updaters ->
            updaters |> collectRecordSetterExpressions

        Application expressions ->
            expressions |> List.concatMap collectLetDeclarationsFromExpression

        CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases
                        |> List.map (\( _, expr ) -> expr)
                   )
                |> List.concatMap collectLetDeclarationsFromExpression

        OperatorApplication _ _ aExpr bExpr ->
            [ aExpr, bExpr ]
                |> List.concatMap collectLetDeclarationsFromExpression

        IfBlock boolExpr thenExpr elseExpr ->
            [ boolExpr, thenExpr, elseExpr ]
                |> List.concatMap collectLetDeclarationsFromExpression

        LambdaExpression lambda ->
            lambda.expression |> collectLetDeclarationsFromExpression

        RecordAccess record _ ->
            record |> collectLetDeclarationsFromExpression

        ParenthesizedExpression expr ->
            expr |> collectLetDeclarationsFromExpression

        Negation expr ->
            expr |> collectLetDeclarationsFromExpression

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        GLSLExpression _ ->
            []

        RecordAccessFunction _ ->
            []

        FunctionOrValue _ _ ->
            []

        Operator _ ->
            []

        PrefixOperator _ ->
            []
