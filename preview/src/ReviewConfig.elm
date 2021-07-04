module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

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
