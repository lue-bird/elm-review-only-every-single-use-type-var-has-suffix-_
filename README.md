# elm-review-highlight-single-use-type-vars

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to make sure single-use type variables are highlighted with the suffix -_.

## why does this exist

-_ at the end of a type variable is a good indication that it is used only in this one place.

Some types have a lot of type variables, most of them only used once.
If you see a -_ you know not to focus on these. Through the review rules you can make sure that this type variable isn't used anywhere else.

### example

Which one is easier to understand?

```elm
at :
    Nat (ArgIn indexMin minLengthMinus1 indexIfN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> element
```
```elm
at :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength_) element
    -> element
```
(from [typesafe-array: Arr.at](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/Arr#at))

Once youre used to this, it can be really nice, similar to

```elm
at :
    Nat (ArgIn _ minLengthMinus1 _)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) _) element
    -> element
```
which sadly doesn't exist 😢

## Provided rules

- [`No.MultiUseTypeVars.EndWith_`](https://package.elm-lang.org/packages/lue-bird/elm-review-highlight-single-use-type-vars/1.0.0/No-MultiUseTypeVars-EndWith_) - Reports multi-use type variables that have a -_ suffix.
- [`SingleUseTypedVars.EndWith_`](https://package.elm-lang.org/packages/lue-bird/elm-review-highlight-single-use-type-vars/1.0.0/SingleUseTypedVars-EndWith_) - Reports single-use type variables that don't have a -_ suffix.

## Configuration

```elm
module ReviewConfig exposing (config)

import No.MultiUse.TypeVar.HasSuffix_
import SingleUseTypedVars.EndWith_
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ SingleUseTypedVars.EndWith_.rule
    , No.MultiUse.TypeVar.HasSuffix_.rule
    ]
```


## When not to use this

You already use the -_ suffix in (multi-use) type variables.
