# elm-review-single-use-type-vars-end-with-underscore

[`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to make sure that type variables which are only used once are highlighted with the suffix -_.

## why does this exist?

-_ at the end of a type variable is a good indication that it is used only in this one place.

Some types have a lot of type variables, most of them only used once.
If you see a -_

- you know not to focus on these
- through the review rule you can make sure that this type variable isn't used anywhere else → 2 type variables can't accidentally be the same.

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

- [`OnlyAllSingleUseTypeVarsEndWith_`](https://package.elm-lang.org/packages/elm-review-single-use-type-vars-end-with-underscore/1.0.0/OnlyAllSingleUseTypeVarsEndWith_)
    - Reports multi-use type variables that have a -_ suffix.
    - Reports single-use type variables that don't have a -_ suffix.

## Configuration

```elm
module ReviewConfig exposing (config)

import OnlyAllSingleUseTypeVarsEndWith_
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ OnlyAllSingleUseTypeVarsEndWith_.rule
    ]
```


## Don't use this

When you already use the -_ suffix in (multi-use) type variables.
