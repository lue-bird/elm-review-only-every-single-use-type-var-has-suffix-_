module SingleUseTypeVars.EndWith_ exposing (rule)

{-|

@docs rule

-}

import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ Every.SingleUse.TypedVar.Has.Suffix_.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template lue-bird/elm-review-highlight-single-use-type-vars/example --rules Every.SingleUse.TypedVar.Has.Suffix_
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Every.SingleUse.TypedVar.Has.Suffix_" ()
        -- Add your visitors
        |> Rule.fromModuleRuleSchema
