module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import UsageScopeLimit


config : List Rule
config =
    [ UsageScopeLimit.for
        [ UsageScopeLimit.modulesEndingWith [ "Internal" ] ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.baseModules ]
        |> UsageScopeLimit.because
            [ """This keeps the `module` structure organized,
never "leaking" any implementation details into other `module`s"""
            ]
    ]


forbiddenFunctionOrValues : List String
forbiddenFunctionOrValues =
    [ -- use tuple destructuring instead
      -- for improved descriptiveness
      "Tuple.first"
    , "Tuple.second"
    , -- use `mapFirst |> mapSecond` instead
      "Tuple.mapBoth"
    , "Basics.always"
    , -- use `String.indexes` instead
      "String.indices"
    , -- use a `case` instead
      "String.isEmpty"
    , "List.isEmpty"
    , "List.tail"
    ]