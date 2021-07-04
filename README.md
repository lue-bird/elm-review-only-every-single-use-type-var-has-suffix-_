# elm-review-only-every-single-use-type-var-has-suffix-_

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`No.MultiUse.TypeVar.Has.Suffix_`](https://package.elm-lang.org/packages/lue-bird/elm-review-only-every-single-use-type-var-has-suffix-_/1.0.0/No-MultiUse-TypeVar-Has-Suffix_) - Reports REPLACEME.
- [`Every.SingleUse.TypedVar.Has.Suffix_`](https://package.elm-lang.org/packages/lue-bird/elm-review-only-every-single-use-type-var-has-suffix-_/1.0.0/Every-SingleUse-TypedVar-Has-Suffix_) - Reports REPLACEME.
- [`Only.Every.SingleUse.TypeVar.Has.Suffix_`](https://package.elm-lang.org/packages/lue-bird/elm-review-only-every-single-use-type-var-has-suffix-_/1.0.0/Only-Every-SingleUse-TypeVar-Has-Suffix_) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import Every.SingleUse.TypedVar.Has.Suffix_
import No.MultiUse.TypeVar.Has.Suffix_
import Only.Every.SingleUse.TypeVar.Has.Suffix_
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Only.Every.SingleUse.TypeVar.Has.Suffix_.rule
    , No.MultiUse.TypeVar.Has.Suffix_.rule
    , Every.SingleUse.TypedVar.Has.Suffix_.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template lue-bird/elm-review-only-every-single-use-type-var-has-suffix-_/example
```
