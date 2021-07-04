module Common exposing (collectTypeVarsFromDeclaration, collectTypeVarsFromExpression, groupTypeVars)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List.Extra as List
import List.NonEmpty


collectTypeVarsFromDeclaration : Node Declaration -> List (Node String)
collectTypeVarsFromDeclaration declaration =
    case Node.value declaration of
        FunctionDeclaration function ->
            function |> collectTypeVarsFromFunction

        CustomTypeDeclaration { generics, constructors } ->
            generics
                ++ (constructors
                        |> List.map (Node.value >> .arguments)
                        |> List.concatMap
                            (List.concatMap collectTypeVarsFromType)
                   )

        PortDeclaration { typeAnnotation } ->
            collectTypeVarsFromType typeAnnotation

        _ ->
            []


collectTypeVarsFromFunction : Function -> List (Node String)
collectTypeVarsFromFunction =
    \{ signature } ->
        case signature of
            Nothing ->
                []

            Just type_ ->
                collectTypeVarsFromType
                    (type_ |> Node.value |> .typeAnnotation)


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
            typeVarIfEndsWith_ (Node.value toExtend)
                :: (fields
                        |> Node.value
                        |> List.map Node.value
                        |> List.map (\( _, type__ ) -> type__)
                        |> List.map collectTypeVarsFromType
                        |> List.concat
                   )


collectTypeVarsFromExpression : Node Expression -> List (Node String)
collectTypeVarsFromExpression expression =
    let
        collectRecordSetterExpressions =
            List.map Node.value
                >> List.map (\( _, expr ) -> expr)
                >> List.concatMap collectTypeVarsFromExpression
    in
    case Node.value expression of
        LetExpression letBlock ->
            (letBlock.expression
                |> collectTypeVarsFromExpression
            )
                ++ (letBlock.declarations
                        |> List.concatMap
                            (\letDeclaration ->
                                case Node.value letDeclaration of
                                    LetFunction function ->
                                        function |> collectTypeVarsFromFunction

                                    LetDestructuring _ expr ->
                                        expr |> collectTypeVarsFromExpression
                            )
                   )

        ListExpr expressions ->
            expressions |> List.concatMap collectTypeVarsFromExpression

        TupledExpression expressions ->
            expressions |> List.concatMap collectTypeVarsFromExpression

        RecordExpr setters ->
            setters |> collectRecordSetterExpressions

        RecordUpdateExpression _ updaters ->
            updaters |> collectRecordSetterExpressions

        Application expressions ->
            expressions |> List.concatMap collectTypeVarsFromExpression

        CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases
                        |> List.map (\( _, expr ) -> expr)
                   )
                |> List.concatMap collectTypeVarsFromExpression

        OperatorApplication _ _ aExpr bExpr ->
            [ aExpr, bExpr ]
                |> List.concatMap collectTypeVarsFromExpression

        IfBlock boolExpr thenExpr elseExpr ->
            [ boolExpr, thenExpr, elseExpr ]
                |> List.concatMap collectTypeVarsFromExpression

        LambdaExpression lambda ->
            lambda.expression |> collectTypeVarsFromExpression

        RecordAccess record _ ->
            record |> collectTypeVarsFromExpression

        ParenthesizedExpression expr ->
            expr |> collectTypeVarsFromExpression

        Negation expr ->
            expr |> collectTypeVarsFromExpression

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


groupTypeVars :
    ({ length : Int } -> Bool)
    -> List (Node comparable)
    -> List { typeVarName : comparable, range : Range }
groupTypeVars isErrorGroup typeVars =
    typeVars
        |> groupBy Node.value
        |> List.filter
            (\list ->
                { length = List.NonEmpty.length list }
                    |> isErrorGroup
            )
        |> List.map
            (\( head, tail ) ->
                { typeVarName = Node.value head
                , range =
                    (head :: tail)
                        |> List.map Node.range
                        |> Range.combine
                }
            )



-- util


groupBy : (a -> comparable_) -> List a -> List ( a, List a )
groupBy toComparable list =
    list
        |> List.sortBy toComparable
        |> List.groupWhile
            (\a b -> toComparable a == toComparable b)
