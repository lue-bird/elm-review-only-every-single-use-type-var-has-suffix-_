module OnlyAllSingleUseTypeVarsEndWith_ exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Review.Fix
import Review.Rule


{-| Reports in types of module scope declarations

  - single-use type variables that don't have a -\_ suffix
  - multi-use type variables that have a -\_ suffix

```
config =
    [ OnlyAllSingleUseTypeVarsEndWith_.rule
    ]
```


### reported

    empty : List element

    drop : Int -> List element_ -> List element_


### not reported

    empty : List element_

    drop : Int -> List element -> List element


## Why you might not want this

  - You want to keep the conventional way of naming type variables for consistency?
  - You already use the -\_ suffix in possibly multi-use type variables?
  - You dislike having possibly multi-use -\_ suffixed type variables in your let declarations?

-}
rule : Review.Rule.Rule
rule =
    Review.Rule.newModuleRuleSchema "OnlyAllSingleUseTypeVarsEndWith_" ()
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.withSimpleDeclarationVisitor
            (\(Elm.Syntax.Node.Node _ declaration) ->
                declaration |> declarationCheck
            )
        |> Review.Rule.fromModuleRuleSchema


declarationCheck : Elm.Syntax.Declaration.Declaration -> List (Review.Rule.Error {})
declarationCheck declaration =
    declaration
        |> collectTypeVarsFromDeclaration
        |> typeVarsCheck


typeVarsCheck :
    { inAnnotation : List (Elm.Syntax.Node.Node String)
    , inLets : List (Elm.Syntax.Node.Node String)
    }
    -> List (Review.Rule.Error {})
typeVarsCheck typeVars =
    let
        typeVarsGrouped =
            typeVars.inAnnotation
                |> typeVariableNodesGroupByName
                |> Dict.foldl
                    (\typeVariableName occurrences soFar ->
                        if typeVariableName |> String.endsWith "_" then
                            let
                                typeVariableWithoutEndingUnderscore : String
                                typeVariableWithoutEndingUnderscore =
                                    typeVariableName |> String.dropRight 1
                            in
                            if isReserved typeVariableWithoutEndingUnderscore then
                                soFar

                            else
                                { withUnderscore =
                                    soFar.withUnderscore
                                        |> Dict.insert typeVariableName occurrences
                                , withoutUnderscore = soFar.withoutUnderscore
                                }

                        else
                            { withUnderscore = soFar.withUnderscore
                            , withoutUnderscore =
                                soFar.withoutUnderscore
                                    |> Dict.insert typeVariableName occurrences
                            }
                    )
                    { withUnderscore = Dict.empty
                    , withoutUnderscore = Dict.empty
                    }
    in
    (typeVarsGrouped.withoutUnderscore
        |> Dict.foldl
            (\typeVariableName ( occurrenceWithoutUnderscoreRange, typeVarOccurrencesWithoutUnderscore ) errorsSoFar ->
                case typeVarOccurrencesWithoutUnderscore of
                    [] ->
                        (Elm.Syntax.Node.Node occurrenceWithoutUnderscoreRange typeVariableName
                            |> singleUseTypeVarWithout_Error typeVars
                        )
                            :: errorsSoFar

                    _ :: _ ->
                        errorsSoFar
            )
            []
    )
        ++ (typeVarsGrouped.withUnderscore
                |> Dict.foldl
                    (\name ( occurrence0, occurrence1Up ) errorsSoFar ->
                        case occurrence1Up of
                            _ :: _ ->
                                multiUseTypeVarsWithUnderscoreError typeVars
                                    { name = name
                                    , occurrences = occurrence0 :: occurrence1Up
                                    }
                                    :: errorsSoFar

                            [] ->
                                errorsSoFar
                    )
                    []
           )


typeVariableNodesGroupByName :
    List (Elm.Syntax.Node.Node String)
    -> Dict String ( Elm.Syntax.Range.Range, List Elm.Syntax.Range.Range )
typeVariableNodesGroupByName typeVariableNodes =
    typeVariableNodes
        |> List.foldl
            (\(Elm.Syntax.Node.Node range name) soFar ->
                soFar
                    |> Dict.update name
                        (\occurrencesOfNameSoFar ->
                            Just
                                (case occurrencesOfNameSoFar of
                                    Nothing ->
                                        ( range, [] )

                                    Just ( occurrence0OfNameSoFar, occurrence1UpOfNameSoFar ) ->
                                        ( range, occurrence0OfNameSoFar :: occurrence1UpOfNameSoFar )
                                )
                        )
            )
            Dict.empty


singleUseTypeVarWithout_Error :
    { inAnnotation : List (Elm.Syntax.Node.Node String)
    , inLets : List (Elm.Syntax.Node.Node String)
    }
    -> Elm.Syntax.Node.Node String
    -> Review.Rule.Error {}
singleUseTypeVarWithout_Error typeVars typeVarNode =
    let
        (Elm.Syntax.Node.Node typeVarRange typeVar) =
            typeVarNode

        typeVarPlusExtraUnderscoreAlreadyExists : Bool
        typeVarPlusExtraUnderscoreAlreadyExists =
            (typeVars.inAnnotation ++ typeVars.inLets)
                |> List.any
                    (\(Elm.Syntax.Node.Node _ element) ->
                        element == (typeVar ++ "_")
                    )
    in
    Review.Rule.errorWithFix
        (singleUseTypeVarDoesntEndWith_ErrorInfo
            { typeVar = typeVar, typeVar_Exists = typeVarPlusExtraUnderscoreAlreadyExists }
        )
        typeVarRange
        (if typeVarPlusExtraUnderscoreAlreadyExists then
            -- in cases like `unwrapResult : Result x x_ -> x_`
            -- we currently don't report an error because we can't provide a fix
            -- Open an issue if you think we should still report it as an error
            --     and prompting to rename the more-underscored type variable
            []

         else
            (typeVarNode
                :: (typeVars.inLets
                        |> List.filter
                            (\(Elm.Syntax.Node.Node _ typeVarInLet) ->
                                typeVarInLet == typeVar
                            )
                   )
            )
                |> List.map
                    (\(Elm.Syntax.Node.Node typeVarInLetRange _) ->
                        Review.Fix.insertAt typeVarInLetRange.end "_"
                    )
        )


singleUseTypeVarDoesntEndWith_ErrorInfo :
    { typeVar : String, typeVar_Exists : Bool }
    -> { message : String, details : List String }
singleUseTypeVarDoesntEndWith_ErrorInfo { typeVar, typeVar_Exists } =
    { message =
        "The type variable `"
            ++ typeVar
            ++ "` isn't marked as single-use with a -_ suffix."
    , details =
        [ biggestReasonOfExistence
        , if typeVar_Exists then
            "Don't rename to `"
                ++ typeVar
                ++ "`. Such a type variable already exists."
                ++ " Try choosing a different name with a -_ suffix."

          else
            "Add the -_ suffix (there's a fix available for that)."
        ]
    }


multiUseTypeVarsWithUnderscoreError :
    { inAnnotation : List (Elm.Syntax.Node.Node String)
    , inLets : List (Elm.Syntax.Node.Node String)
    }
    -> { name : String, occurrences : List Elm.Syntax.Range.Range }
    -> Review.Rule.Error {}
multiUseTypeVarsWithUnderscoreError typeVars culprits =
    let
        culpritsRange : Elm.Syntax.Range.Range
        culpritsRange =
            culprits.occurrences
                |> Elm.Syntax.Range.combine

        typeVarWithout_Exists : Bool
        typeVarWithout_Exists =
            (typeVars.inAnnotation ++ typeVars.inLets)
                |> List.any
                    (\(Elm.Syntax.Node.Node _ element) ->
                        element == (culprits.name |> String.dropRight 1)
                    )
    in
    Review.Rule.errorWithFix
        (multiUseTypeVarsEndWith_ErrorInfo
            { typeVar = culprits.name
            , typeVarWithout_Exists = typeVarWithout_Exists
            }
        )
        culpritsRange
        (if typeVarWithout_Exists then
            []

         else
            (culprits.occurrences
                ++ (typeVars.inLets
                        |> List.filterMap
                            (\(Elm.Syntax.Node.Node typeVarRange typeVarInLet) ->
                                if typeVarInLet == culprits.name then
                                    Just typeVarRange

                                else
                                    Nothing
                            )
                   )
            )
                |> List.map
                    (\range ->
                        let
                            rangeEnd : Elm.Syntax.Range.Location
                            rangeEnd =
                                range.end
                        in
                        Review.Fix.removeRange
                            { start = { rangeEnd | column = rangeEnd.column - 1 }
                            , end = range.end
                            }
                    )
        )


multiUseTypeVarsEndWith_ErrorInfo :
    { typeVar : String, typeVarWithout_Exists : Bool }
    -> { message : String, details : List String }
multiUseTypeVarsEndWith_ErrorInfo { typeVar, typeVarWithout_Exists } =
    { message =
        "The type variable `"
            ++ typeVar
            ++ "` is used in multiple places,"
            ++ " despite being marked as single-use with the -_ suffix."
    , details =
        [ biggestReasonOfExistence
        , "Rename one of them if this was an accident. "
        , "If it wasn't an accident, "
            ++ (if typeVarWithout_Exists then
                    "choose a different name (`"
                        ++ (typeVar |> String.dropRight 1)
                        ++ "` already exists)."

                else
                    "remove the -_ suffix (there's a fix available for that)."
               )
        ]
    }


biggestReasonOfExistence : String
biggestReasonOfExistence =
    "-_ at the end of a type variable is a good indication that it is used only in this one place."


isReserved : String -> Bool
isReserved name =
    case name of
        "module" ->
            True

        "exposing" ->
            True

        "import" ->
            True

        "as" ->
            True

        "if" ->
            True

        "then" ->
            True

        "else" ->
            True

        "let" ->
            True

        "in" ->
            True

        "case" ->
            True

        "of" ->
            True

        "port" ->
            True

        "type" ->
            True

        -- "infix", "infixr", "infixl", "alias" are not reserved keywords
        "where" ->
            True

        _ ->
            False


collectTypeVarsFromDeclaration :
    Elm.Syntax.Declaration.Declaration
    ->
        { inAnnotation : List (Elm.Syntax.Node.Node String)
        , inLets : List (Elm.Syntax.Node.Node String)
        }
collectTypeVarsFromDeclaration declaration =
    let
        noTypeVars =
            { inAnnotation = [], inLets = [] }
    in
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            { inAnnotation =
                case function.signature of
                    Nothing ->
                        []

                    Just (Elm.Syntax.Node.Node _ { typeAnnotation }) ->
                        typeAnnotation |> allTypeVarsInType
            , inLets =
                let
                    allTypeVarsInExpression :
                        Elm.Syntax.Expression.Expression
                        -> List (Elm.Syntax.Node.Node String)
                    allTypeVarsInExpression expression =
                        (case expression of
                            Elm.Syntax.Expression.LetExpression letBlock ->
                                let
                                    typesInLetDeclaration :
                                        Elm.Syntax.Expression.LetDeclaration
                                        -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
                                    typesInLetDeclaration letDeclaration =
                                        case letDeclaration of
                                            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                                letValueOrFunctionDeclaration.signature
                                                    |> Maybe.map
                                                        (\(Elm.Syntax.Node.Node _ signature) ->
                                                            signature.typeAnnotation
                                                        )

                                            Elm.Syntax.Expression.LetDestructuring _ _ ->
                                                Nothing
                                in
                                letBlock.declarations
                                    |> List.concatMap
                                        (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                            case letDeclaration |> typesInLetDeclaration of
                                                Nothing ->
                                                    []

                                                Just types ->
                                                    types |> allTypeVarsInType
                                        )

                            _ ->
                                []
                        )
                            ++ (expression
                                    |> expressionDirectSubs
                                    |> List.concatMap
                                        (\(Elm.Syntax.Node.Node _ sub) ->
                                            sub |> allTypeVarsInExpression
                                        )
                               )
                in
                function.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> Elm.Syntax.Node.value
                    |> allTypeVarsInExpression
            }

        Elm.Syntax.Declaration.PortDeclaration { typeAnnotation } ->
            { inAnnotation = typeAnnotation |> allTypeVarsInType
            , inLets = []
            }

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            noTypeVars

        Elm.Syntax.Declaration.AliasDeclaration _ ->
            noTypeVars

        Elm.Syntax.Declaration.Destructuring _ _ ->
            noTypeVars

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            noTypeVars


expressionDirectSubs :
    Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionDirectSubs expression =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.RecordAccess record _ ->
            [ record ]

        Elm.Syntax.Expression.ParenthesizedExpression expression_ ->
            [ expression_ ]

        Elm.Syntax.Expression.Negation expression_ ->
            [ expression_ ]

        Elm.Syntax.Expression.OperatorApplication _ _ leftExpression rightExpression ->
            [ leftExpression, rightExpression ]

        Elm.Syntax.Expression.IfBlock predExpr thenExpr elseExpr ->
            [ predExpr, thenExpr, elseExpr ]

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.Application expressions ->
            expressions

        Elm.Syntax.Expression.RecordExpr fields ->
            fields
                |> List.map
                    (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                        value
                    )

        Elm.Syntax.Expression.RecordUpdateExpression record updaters ->
            (record
                |> Elm.Syntax.Node.map
                    (Elm.Syntax.Expression.FunctionOrValue [])
            )
                :: (updaters
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ ( _, newValue )) ->
                                newValue
                            )
                   )

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases
                        |> List.map
                            (\( _, caseResult ) ->
                                caseResult
                            )
                   )

        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.expression
                :: (letBlock.declarations
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        letValueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .expression

                                    Elm.Syntax.Expression.LetDestructuring _ expression_ ->
                                        expression_
                            )
                   )


allTypeVarsInType :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node String)
allTypeVarsInType type_ =
    case Elm.Syntax.Node.value type_ of
        Elm.Syntax.TypeAnnotation.Unit ->
            []

        Elm.Syntax.TypeAnnotation.GenericType typeVarName ->
            [ Elm.Syntax.Node.Node (Elm.Syntax.Node.range type_) typeVarName ]

        Elm.Syntax.TypeAnnotation.Typed _ typeArguments ->
            typeArguments
                |> List.concatMap allTypeVarsInType

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation a b ->
            (a |> allTypeVarsInType)
                ++ (b |> allTypeVarsInType)

        Elm.Syntax.TypeAnnotation.Tupled innerTypes ->
            innerTypes
                |> List.concatMap allTypeVarsInType

        Elm.Syntax.TypeAnnotation.Record fields ->
            fields
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ ( _, type__ )) ->
                        type__ |> allTypeVarsInType
                    )

        Elm.Syntax.TypeAnnotation.GenericRecord toExtend (Elm.Syntax.Node.Node _ fields) ->
            toExtend
                :: (fields
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ ( _, type__ )) ->
                                type__ |> allTypeVarsInType
                            )
                   )
