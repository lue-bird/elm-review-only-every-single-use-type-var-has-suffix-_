# [elm-review-single-use-type-vars-end-with-underscore](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-single-use-type-vars-end-with-underscore/latest/)

The [`elm-review`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/) rule
🔧[`OnlyAllSingleUseTypeVarsEndWith_`](OnlyAllSingleUseTypeVarsEndWith_)
enforces that type variables which are only used once
in annotations of module scope declaration
are marked with the suffix -\_ and other type variables are not.

## why would you want this?

You can use -\_ at the end of a type variable to indicate that it is used only in this one place.

Some types have a lot of type variables, most of them only used once.
If you see a -\_, you know not to focus on these
  - -\_ in the result: can be inferred as anything
  - -\_ in an argument: anything allowed
  
The review rule makes sure that this type variable isn't used anywhere else
so that 2 type variables can't accidentally be the same.

```elm
httpRequestSend :
    { url : String
    , method : String
    , onResponse : Bytes -> msg
    , headers :
          List
              { name : String
              , value : String
              }
    , body : Maybe Bytes
    }
    -> Task error_ msg
```
We can see at a glance that `error_` isn't related to an argument,
it's free to take on any type whereas `msg` is something an argument has to provide.

Likewise, if we see `-_` in an argument, we also know it's not used in any other part of the type.
```elm
nodeCount :
    lockingTag
    -> TaggedTree label_ lockingTag
    -> Int
```

## configure

```elm
module ReviewConfig exposing (config)

import OnlyAllSingleUseTypeVarsEndWith_
import Review.Rule

config : List Review.Rule.Rule
config =
    [ OnlyAllSingleUseTypeVarsEndWith_.rule
    ]
```

## why you might not want this

  - The conventional way of naming type variables is used consistently 
    in pretty much all existing elm code and is already familiar to elm folks
  - you already use -/_ for separate reasons in some type variables
  - since `let` declaration annotations may need to refer to types that are single-use in the module-scope declaration annotation, this review rule does not consider `let` type variables at all which might be confusing

Ultimately, the solution to fix all three
would be an editor extension that de-emphasizes (less contrast, ...) single-use type variables.
