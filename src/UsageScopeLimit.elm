module UsageScopeLimit exposing
    ( for
    , LimitedScope(..), toInside, toOutsideOf, ConfigMissingReason(..)
    , because
    , ModuleScope, Included(..), Limited, ModuleScopeCommon
    , specific, directories, packages
    , modules, modulesStartingWith, modulesEndingWith
    , baseModules
    , regexForbid, boolTypeForbid
    )

{-|


## When to use

Useful to restrict using certain named things to certain modules;
especially to guarantee consistency, e.g. for input fields or design elements.

> [The `Config` documentation](UsageScopeLimit#Config) covers **multiple examples and use-cases → Give them a 👀**

This sense more often than you may think:

  - using multiple `source-directories`/"namespaces"
      - limit each separate thing to its own space & the main modules

  - forbid things completely
      - for example [`regexForbid`](#regexForbid), [`boolTypeForbid`](#boolTypeForbid)

  - forbid "namespaces" to import from others

  - ...


## When not to use

  - You want to allow minor inconsistencies to stay flexible
  - You're using different techniques to keep consistency (through types, packages, ...)
  - There are rules out there specific to your demands with extra functionality like **fixes**, ...
      - [`sparksp/elm-review-always`](https://package.elm-lang.org/packages/sparksp/elm-review-always/latest/NoAlways)
      - [`jfmengels/elm-review-common`: `NoDeprecated`](https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoDeprecated)
      - [`leojpod/review-no-empty-html-text`](https://package.elm-lang.org/packages/leojpod/review-no-empty-html-text/latest)
      - ...

Q: Why can't you attach fixes?

A: It would be neat to simply have

  - `String.indices` → `String.indexes`
  - `Basics.always` → `\_ ->`
  - `Tuple.mapBoth` → `mapFirst |> mapSecond`
  - `Basics.(++)` → `List.concat`/`String.concat`
  - `mapN` → `andMap |> andMap |> ...`
  - `rgb(a)255` → `rgb(a)`
  - ...

while those are easy fixes for a human,
you will be better off writing a complete custom `elm-review` `Rule`
to collect context, extract source code and arguments, generate code etc.!

You can always fork/clone `elm-review-usage-scope` to create your custom rule _with fixes_ :)
Please do 🐦


## configure

@docs for
@docs LimitedScope, toInside, toOutsideOf, ConfigMissingReason
@docs because


### scope

@docs ModuleScope, Included, Limited, ModuleScopeCommon
@docs specific, directories, packages
@docs modules, modulesStartingWith, modulesEndingWith
@docs baseModules


### preset

Easy-to-implement-yourself templates for common [`Config`](#Config)s.
Submit yours in a PR for everyone to profit 🙌

@docs regexForbid, boolTypeForbid

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as Type exposing (TypeAnnotation)
import Help exposing (alterCombined, areAll, backQuote, declarationDirectNamedThings, exposeName, expressionDirectNamedThings, isOneOf, moduleNameToString, packageOriginLookup, patternChildren, qualifiedToString, setEnumerate, setToFilled, typeChildren)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project
import Review.Project.Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)



-- configure


{-| configuration → Start by specifying [`for`](#for) what there's a limit in usage scope.


## example: module in a "namespace" importing from another "namespace"

    UsageScopeLimit.for
        [ UsageScopeLimit.modulesStartingWith [ "App.View" ] ]
        |> UsageScopeLimit.toOutsideOf
            [ UsageScopeLimit.modulesStartingWith [ "App.Data" ] ]
        |> UsageScopeLimit.because
            [ """The namespace structure forms a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) of module dependencies:
    `App.View` is irrelevant and therefore forbidden for `App.Data`."""
            ]


### reported

    module App.Data.Image exposing (bird)
    import App.View.Image

fails: we've limited `module`s in `App.Data` to use `App.View...`.


### accepted

    module App.View.Image exposing (bird)
    import App.Data.Image

passes: we've only limited `module`s in `App.Data` to use `App.View...`, not the other way around.


## example: forbidding named things outside of an allowed scope

    UsageScopeLimit.for
        [ UsageScopeLimit.specific [ ( "Html, "input" ) ] ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.modulesStartingWith [ "View.Input" ] ]
        |> UsageScopeLimit.because
            [ """At fruits.com, we think it is best not to mix raw `elm/html` with our custom ui;
    keeping things simple and consistent."""
            , """The API is very similar, but instead of using the `Html` module, use the `View.Input`.
    Style is then defined using the `View.Input.style` function."""
            ]


### reported

    module Main exposing (main)

    import Html exposing (input)

    main =
        input [] []

`input` was exposed from `Html` → forbidden outside of `View.Input...`


### accepted

    module View.Input.Text exposing (single)

    import Html

    single =
        Html.input [] []

`Html.input` is allowed inside `View.Input...`


## example: forbidding specific

    UsageScopeLimit.for
        [ UsageScopeLimit.specific
            [ ( "Tuple", "first" ), ( "Tuple", "second" ) ]
        ]
        |> UsageScopeLimit.toInside []
        |> UsageScopeLimit.because
            [ "tuple destructuring is more descriptive, for example `\\( key, _ ) -> key`"
            , """Try to reach for records whenever possible...
    but when life gives you tuples 🍋,
    at least take the time to name the pattern variables."""
            ]

In the future this functionality should receive its own rule, additionally checking stuff like

  - `Tuple.mapFirst f`/`Tuple.mapSecond f` must take named arguments: `Tuple.mapFirst (\key -> key |> f)`
  - `Tuple.mapBoth` forbidden
  - ...

Maybe a nice rule to get into writing `elm-review` rules? (can recommend)


## example: `module` usage limited to base

    UsageScopeLimit.for
        [ UsageScopeLimit.modulesEndingWith [ "Internal" ] ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.baseModules ]
        |> UsageScopeLimit.because
            [ """This keeps the `module` structure organized,
    never "leaking" any implementation details into other `module`s"""
            ]


### try it out without installation

```bash
elm-review --template lue-bird/elm-review-usage-scope-limit/example/internal-only-used-in-base
```


### accepted

    module Structure exposing (create)
    import Structure.Internal exposing (change)

Non-internal `module` has the same base `Structure`.


### reported

    module Structure exposing (create)
    import Structure.Internal.Element exposing (change)

Only `module Structure.Internal` can `import`.

-}
type alias Config =
    RecordWithoutConstructorFunction
        { limited : Limited
        , limitedScope : LimitedScope
        , reasonDetails : List String
        }


{-| Specify all named things with a scope to limit. See [`Limited`](#Limited)

Next step is specifying the `UsageScopeLimit`: `|>`[`to`](#to)

-}
for :
    List (WithContext Limited {} -> WithContext Limited context)
    -> WithContext Limited context
for namedThingsWithLimitedScope =
    WithoutContext noneLimited
        |> alterCombined
            (namedThingsWithLimitedScope
                |> List.map
                    (\f ->
                        \(WithoutContext scope) ->
                            scope |> WithoutContext |> f
                    )
            )


noneLimited : Limited
noneLimited =
    { specific = Set.empty
    , modules = Set.empty
    , moduleStarts = Set.empty
    , moduleEndings = Set.empty
    , packages = Set.empty
    , directories = Set.empty
    }


{-| In what [modules](#ModuleScope) the specific are still allowed in.

[Configure](#Config) using

  - [`UsageScope.toInside`](#toInside)
  - [`UsageScopeLimit.toOutsideOf`](#toOutsideOf)

-}
type LimitedScope
    = Inside ModuleScope
    | OutsideOf ModuleScope


{-| Incomplete [`Config`](#Config), missing only a detailed reason → [`because`](#because)
-}
type ConfigMissingReason
    = ConfigMissingReason
        { limited : Limited
        , limitedScope : LimitedScope
        }


{-| Specify a scope to limit specified named things to. See [`ModuleScope`](#ModuleScope).

Forbid any uses using

    ... |> UsageScopeLimit.toInside []

Next step is _explaining_ the `UsageScopeLimit`: `|>`[`because`](#because)

If you're motivated to see
how it's guaranteed that `baseModules` is only available
when `modules-Starting/Ending-With` are added,
→ [`WithContext`](#WithContext)


## example: `mdgriffith/elm-ui` `Color`

    UsageScopeLimit.for
        [ UsageScopeLimit.specific
            [ ( "Element", "Color" )
            , ( "Element", "rgb" )
            , ( "Element", "rgba" )
            , ( "Element", "rgb255" )
            , ( "Element", "rgba255" )
            ]
        ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.modules [ "Element.Extra" ] ]
        |> UsageScopeLimit.because
            [ """Use the `Element.Extra` alternatives instead."""
            , """It uses `avh4/elm-color` instead of `elm-ui`'s custom ones
    to enable defining global colors and spare translations."""
            , "[`elm-ui` 2 will switch to `avh4/elm-color` anyway](https://github.com/mdgriffith/design-discussion-elm-ui-2/issues/2)"
            ]


### accepted

    module Main exposing (main)

    import Element.Extra as Element

    white =
        Element.rgb 1 1 1

`Element` is recognized as an import alias of `Element.Extra` which doesn't have usage-scope-limited members.


### reported

    module Element.Extra.Color
    import Element exposing (Color)

`Element.Color` is forbidden outside of `module Element.Extra`, no `module`s with the same start!

-}
toInside :
    List
        (WithContext ModuleScope context
         -> WithContext ModuleScope context
        )
    -> WithContext Limited context
    -> ConfigMissingReason
toInside limitedScope =
    \configMissingReason ->
        ConfigMissingReason
            { limited =
                configMissingReason |> withoutContext
            , limitedScope =
                Inside
                    (WithoutContext nowhere
                        |> alterCombined limitedScope
                        |> withoutContext
                    )
            }


{-| Specify a scope to forbid specified named things in. See [`ModuleScope`](#ModuleScope).

Next step is _explaining_ the `UsageScopeLimit`: `|>`[`because`](#because)

If you're motivated to see
how it's guaranteed that `baseModules` is only available
when `modules-Starting/Ending-With` are added,
→ [`WithContext`](#WithContext)

-}
toOutsideOf :
    List
        (WithContext ModuleScope context
         -> WithContext ModuleScope context
        )
    -> WithContext Limited context
    -> ConfigMissingReason
toOutsideOf limitedScope =
    \configMissingReason ->
        ConfigMissingReason
            { limited =
                configMissingReason |> withoutContext
            , limitedScope =
                OutsideOf
                    (WithoutContext nowhere
                        |> alterCombined limitedScope
                        |> withoutContext
                    )
            }


nowhere : ModuleScope
nowhere =
    { modules = Set.empty
    , baseModules = Included
    , moduleStarts = Set.empty
    , moduleEndings = Set.empty
    , directories = Set.empty
    }


{-| Limit specific named things:

  - values like `( "Basics", "pi" )`
  - functions like `( "Tuple", "first" )`
  - operators like `( "Parser", "|=" )`
  - patterns like `( "Result", "Ok" )`
  - types like `( "Result", "Result" )`

-}
specific :
    List ( String, String )
    -> WithContext Limited context
    -> WithContext Limited context
specific namedThings =
    withContextMap
        (\scope ->
            { scope
                | specific =
                    scope.specific
                        |> Set.union (namedThings |> Set.fromList)
            }
        )


{-| Limit specific package repositories like `( "elm", "json" )`, `( "elm", "regex" )`
-}
packages :
    List ( String, String )
    -> WithContext Limited context
    -> WithContext Limited context
packages packageNames =
    withContextMap
        (\scope ->
            { scope
                | packages =
                    scope.packages
                        |> Set.union (packageNames |> Set.fromList)
            }
        )


{-| Include specific `module`s like `"String"`, `"Scroll.Extra"`
-}
modules :
    List String
    -> WithContext (ModuleScopeCommon scope) context
    -> WithContext (ModuleScopeCommon scope) context
modules modulesIncluded =
    withContextMap
        (\scope ->
            { scope
                | modules =
                    scope.modules
                        |> Set.union (modulesIncluded |> Set.fromList)
            }
        )


{-| Include specific directories like `"unpublished-package/src/"`, `"generated/src/"`
-}
directories :
    List String
    -> WithContext (ModuleScopeCommon scope) context
    -> WithContext (ModuleScopeCommon scope) context
directories directoriesIncluded =
    withContextMap
        (\scope ->
            { scope
                | directories =
                    scope.directories
                        |> Set.union (directoriesIncluded |> Set.fromList)
            }
        )


{-| Include `module` that start with one of the given starts.

    "Page.Data", "Page.Ui" are UsageScopeLimit.modulesStartingWith [ "Page" ]

[`UsageScopeLimit.baseModules`](#baseModules) would be

    "Data", "Ui"

-}
modulesStartingWith :
    List String
    -> WithContext (ModuleScopeCommon scope) context
    -> WithContext (ModuleScopeCommon scope) { context | limitedHaveBase : () }
modulesStartingWith moduleStartsIncluded =
    withContextMap
        (\scope ->
            { scope
                | moduleStarts =
                    scope.moduleStarts
                        |> Set.union
                            (moduleStartsIncluded
                                |> List.map (\ending -> ending ++ ".")
                                |> Set.fromList
                            )
            }
        )


{-| Include `module`s that end with one of the given endings.
-}
modulesEndingWith :
    List String
    -> WithContext (ModuleScopeCommon scope) context
    -> WithContext (ModuleScopeCommon scope) { context | limitedHaveBase : () }
modulesEndingWith moduleEndingsIncluded =
    \(WithoutContext scope) ->
        { scope
            | moduleEndings =
                scope.moduleEndings
                    |> Set.union
                        (moduleEndingsIncluded
                            |> List.map (\ending -> "." ++ ending)
                            |> Set.fromList
                        )
        }
            |> WithoutContext


{-| Add a phantom context.

[`UsageScopeLimit.Config`](#Config) uses this to ensure that
[`baseModules`](#baseModules) can only be used
if either [`modulesEndingWith`](#modulesEndingWith) or [`modulesStartingWith`](#modulesStartingWith) is limited:

    to-Inside/OutsideOf (simplified) :
        List
            (WithContext ... {}
             -> WithContext ... context
            )
        -> List
            (WithContext ... context
             -> WithContext ... context
            )
        -> ...

    module/specific : ...
        -> WithContext ... context
        -> WithContext ... context

    modules-Starting/Ending-With : ...
        -> WithContext ... context
        -> WithContext ... { context | limitedHaveBase : () }

    baseModules :
        WithContext ... { context | limitedHaveBase : () }
        -> WithContext ... { context | limitedHaveBase : () }

The idea is quite similar to the [phantom builder pattern](https://slides.com/jfmengels/phantom-builder-pattern/).

Here, `context` will either

  - no `modules-Starting/Ending-With` were added
      - → `context` stays `{}`
      - → `baseModules` can't be used
  - `modules-Starting/Ending-With` were added
      - → `context` goes to `{ limitedHaveBase : () }`
      - → `baseModule` can be used

-}
type WithContext value context
    = WithoutContext value


withoutContext : WithContext value context -> value
withoutContext =
    \(WithoutContext value) -> value


withContextMap :
    (value -> mappedValue)
    -> WithContext value context
    -> WithContext mappedValue mappedContext
withContextMap valueChange =
    \(WithoutContext value) ->
        value
            |> valueChange
            |> WithoutContext


{-| Include the `module`s without one of the starts or endings specified earlier.

    UsageScopeLimit.modulesStartingWith [ "Page" ]
    include "Page.Data", "Page.Ui", not "PageNav"

    UsageScopeLimit.baseModules
    include "Data", "Ui", not "Nav"

This allows functionality like [`kress95/elm-review-indirect-internal`](https://package.elm-lang.org/packages/kress95/elm-review-indirect-internal/latest/):

    UsageScopeLimit.for
        [ UsageScopeLimit.modulesEndingWith [ "Internal" ] ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.baseModules ]
        |> UsageScopeLimit.because
            [ "`module`s other than `X` aren't allowed to access `X.Internal` things"
            , """This keeps the `module` structure organized,
    never "leaking" any implementation details into other `module`s"""
            ]

If you're motivated to see
how it's guaranteed that `baseModules` is only available
when `modules-Starting/Ending-With` are added,
→ [`WithContext`](#WithContext)

-}
baseModules :
    WithContext ModuleScope { context_ | limitedHaveBase : () }
    -> WithContext ModuleScope { context_ | limitedHaveBase : () }
baseModules =
    withContextMap (\scope -> { scope | baseModules = Included })


{-| Provide details on why this `UsageScopeLimit` exists.

    import UsageScopeLimit

    UsageScopeLimit.for
        [ UsageScopeLimit.specific [ ( "Html", "input" ) ] ]
        |> UsageScopeLimit.toInside
            [ UsageScopeLimit.modules [ "View.Input" ] ]
        |> UsageScopeLimit.because
            [ "At fruits.com, we think it is best not to mix raw `elm/html` with our custom ui; keeping things simple and consistent."
            , "The API is very similar, but instead of using the `Html` module, use the `View.Input`. Style is then defined using the `View.Input.style` function."
            ]

-}
because :
    List String
    -> ConfigMissingReason
    -> Rule
because reasonDetails =
    \(ConfigMissingReason { limited, limitedScope }) ->
        case reasonDetails |> String.concat of
            "" ->
                { message = "Reason details missing on custom `UsageScopeLimit` rules."
                , details =
                    [ "Adding details helps explaining why this rule is in place and how to fix it!"

                    -- Necessary because [we luckily don't have disable comments](https://jfmengels.net/disable-comments/)
                    ]
                }
                    |> Rule.configurationError ruleName

            _ ->
                { limited = noneLimited
                , limitedScope = limitedScope
                , reasonDetails = reasonDetails
                }
                    |> ruleImplementation


{-| `module` constrains that can be included

  - in [`ModuleScope`](#ModuleScope) or
  - in what is [`Limited`](#Limited)

To [configure](#Config):

  - [`modules`](#modules)
  - [`modulesStartingWith`](#modulesStartingWith)
  - [`modulesEndingWith`](#modulesEndingWith)

-}
type alias ModuleScopeCommon scope =
    { scope
        | modules : Set String
        , moduleStarts : Set String
        , moduleEndings : Set String
        , directories : Set String
    }


{-| Every named thing whose usage scope will limited:

  - [`specific`](#specific) named things
  - [`modules`](#modules)
  - [`modulesStartingWith`](#modulesStartingWith)
  - [`modulesEndingWith`](#modulesEndingWith)
  - [`directories`](#directories)
  - [`packages`](#packages)

-}
type alias Limited =
    ModuleScopeCommon
        { specific : Set ( String, String )
        , packages : Set ( String, String )
        }


{-| Every named thing whose usage scope will limited:

  - [`modules`](#modules)
  - [`baseModules`](#ModuleBase)
  - [`modulesStartingWith`](#modulesStartingWith)
  - [`modulesEndingWith`](#modulesEndingWith)
  - [`directories`](#directories)

-}
type alias ModuleScope =
    ModuleScopeCommon
        { baseModules : Included
        }


{-| As it reads: Whether it's a part or not.

Used in to configure if `baseModules` are `Included` in the [`ModuleScope`](#ModuleScope).

-}
type Included
    = Included
    | NotIncluded


{-| Forbid any `elm/regex` members in the projects.
Complete replacement for [`ContaSystemer/elm-review-no-regex`](https://package.elm-lang.org/packages/ContaSystemer/elm-review-no-regex/latest/NoRegex),
implemented as

    UsageScopeLimit.for
        [ UsageScopeLimit.packages [ ( "elm", "regex" ) ] ]
        |> UsageScopeLimit.toInside []
        |> UsageScopeLimit.because
            [ [ "As mentioned in the [`Regex` package readme](https://package.elm-lang.org/packages/elm/regex/latest/),"
              , " `elm/parser` makes your code easier to read and maintain."
              , "It's safer, too: [\"parse, don't validate\"](https://elm-radio.com/episode/parse-dont-validate/)"
              ]
                |> String.concat
            ]

nudging you towards not reaching for `Regex` easily and everywhere. See [`elm-review suppress`](https://jfmengels.net/stop-the-bleed/).

-}
regexForbid : Rule
regexForbid =
    for [ packages [ ( "elm", "regex" ) ] ]
        |> toInside []
        |> because
            [ [ "As mentioned in the [`Regex` package readme](https://package.elm-lang.org/packages/elm/regex/latest/),"
              , " `elm/parser` makes your code easier to read and maintain."
              , "It's safer, too: [\"parse, don't validate\"](https://elm-radio.com/episode/parse-dont-validate/)"
              ]
                |> String.concat
            ]


{-| Forbid `Basics.Bool` in your project.

    UsageScopeLimit.for
        [ UsageScopeLimit.specific [ ( "Basics", "Bool" ) ] ]
        |> UsageScopeLimit.toInside []
        |> UsageScopeLimit.because
            [ [ [ "Programmers coming from JavaScript, Java, ... tend to reach for boolean values way too often in Elm."
                , " Using a [variant `type`](https://guide.elm-lang.org/types/custom_types.html) is more clearer and more reliable."
                , " Learn more from [\"Solving the Boolean Identity Crisis\" by Jeremy Fairbank](https://www.youtube.com/watch?v=6TDKHGtAxeg&t=85s)"
                , " and from [\"Making Impossible States Impossible\" by Richard Feldman](https://www.youtube.com/watch?v=IcgmSRJHu_8&t=74s)."
                ]
                    |> String.concat
              , "The general problem is [\"primitive obsession\"](https://elm-radio.com/episode/primitive-obsession/)."
              ]
                |> String.concat
            ]

nudging you towards not reaching for `Bool` easily and everywhere. See [`elm-review suppress`](https://jfmengels.net/stop-the-bleed/).

-}
boolTypeForbid : Rule
boolTypeForbid =
    for [ specific [ ( "Basics", "Bool" ) ] ]
        |> toInside []
        |> because
            [ [ [ "Programmers coming from JavaScript, Java, ... tend to reach for boolean values way too often in Elm."
                , " Using a [variant `type`](https://guide.elm-lang.org/types/custom_types.html) is more clearer and more reliable."
                , " Learn more from [\"Solving the Boolean Identity Crisis\" by Jeremy Fairbank](https://www.youtube.com/watch?v=6TDKHGtAxeg&t=85s)"
                , " and from [\"Making Impossible States Impossible\" by Richard Feldman](https://www.youtube.com/watch?v=IcgmSRJHu_8&t=74s)."
                ]
                    |> String.concat
              , "The general problem is [\"primitive obsession\"](https://elm-radio.com/episode/primitive-obsession/)."
              ]
                |> String.concat
            ]



-- rule


ruleName : String
ruleName =
    "UsageScopeLimit"


ruleImplementation : Config -> Rule
ruleImplementation config =
    Rule.newProjectRuleSchema ruleName initialProjectContext
        |> Rule.withDependenciesProjectVisitor
            (\deps context ->
                ( [], context |> dependenciesReview deps )
            )
        |> Rule.withModuleVisitor
            (\moduleSchema ->
                moduleSchema
                    |> -- Q: Redundant because each named thing will be checked anyway?
                       -- A: No.
                       --   - forbidden unused imports should still be reported
                       --   - better error locations
                       Rule.withImportVisitor
                        (\importNode context ->
                            ( importNode
                                |> importReview { config = config, context = context }
                            , context
                            )
                        )
                    |> Rule.withExpressionEnterVisitor
                        (\expressionNode context ->
                            ( expressionNode
                                |> expressionReview { config = config, context = context }
                            , context
                            )
                        )
                    |> Rule.withDeclarationEnterVisitor
                        (\(Node _ declaration) context ->
                            ( declaration
                                |> declarationReview { config = config, context = context }
                            , context
                            )
                        )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = initModuleContext
            , fromModuleToProject =
                Rule.initContextCreator (\_ -> initialProjectContext)
            , foldProjectContexts =
                \projectContext0 projectContext1 ->
                    { packageOriginLookup =
                        projectContext0.packageOriginLookup
                            |> Dict.union projectContext1.packageOriginLookup
                    }
            }
        |> Rule.fromProjectRuleSchema
        |> Rule.filterErrorsForFiles
            (\path ->
                -- if imported module is from limited directory
                -- check if current directory is in limitedScope.directories
                case config.limitedScope of
                    Inside allowed ->
                        setAny
                            (\directoryInsideLimitedScope ->
                                path |> String.startsWith directoryInsideLimitedScope
                            )
                            allowed.directories
                            |> not

                    OutsideOf forbidden ->
                        setAny
                            (\directoryInsideLimitedScope ->
                                path |> String.startsWith directoryInsideLimitedScope
                            )
                            forbidden.directories
            )


type alias ProjectContext =
    RecordWithoutConstructorFunction
        { packageOriginLookup : Dict String { package : ( String, String ) }
        }


initialProjectContext : ProjectContext
initialProjectContext =
    { packageOriginLookup = Dict.empty
    }


type alias ModuleContext =
    RecordWithoutConstructorFunction
        { moduleName : String
        , moduleOriginLookup : ModuleNameLookupTable
        , packageOriginLookup : Dict String { package : ( String, String ) }
        }


initModuleContext : Rule.ContextCreator ProjectContext ModuleContext
initModuleContext =
    Rule.initContextCreator
        (\meta moduleOriginLookup { packageOriginLookup } ->
            { moduleName = meta |> Rule.moduleNameFromMetadata |> moduleNameToString
            , moduleOriginLookup = moduleOriginLookup
            , packageOriginLookup = packageOriginLookup
            }
        )
        |> Rule.withMetadata
        |> Rule.withModuleNameLookupTable


dependenciesReview :
    Dict String Review.Project.Dependency.Dependency
    -> ProjectContext
    -> ProjectContext
dependenciesReview dependencyProjects =
    \context ->
        { context
            | packageOriginLookup =
                dependencyProjects |> packageOriginLookup
        }


importReview :
    { config : Config, context : ModuleContext }
    -> Node Import
    -> List (Rule.Error {})
importReview { config, context } importNode =
    let
        { limitedScope, limited } =
            config

        importedModuleNameNode =
            importNode
                |> Node.value
                |> .moduleName
                |> Node.map moduleNameToString

        importedModule =
            importedModuleNameNode |> Node.value

        reviewedModuleIsInsideLimitedScopeOfImportedModule =
            moduleIsInsideLimitedScope
                { limitedModule = importedModule
                , limitedScope = limitedScope
                , limitedStarts = limited.moduleStarts
                , limitedEndings = limited.moduleEndings
                }

        isLimitedPackage =
            \module_ ->
                case Dict.get importedModule context.packageOriginLookup of
                    Just importModuleFromPackage ->
                        Set.member
                            importModuleFromPackage.package
                            config.limited.packages

                    Nothing ->
                        False

        errors =
            if Debug.todo "check if import directory is limited" then
                [ importedModuleNameNode ]

            else if context.moduleName |> reviewedModuleIsInsideLimitedScopeOfImportedModule then
                []

            else if importedModule |> isLimitedPackage then
                [ importedModuleNameNode ]

            else if importedModule |> moduleIsInScope limited then
                [ importedModuleNameNode ]

            else
                let
                    exposed =
                        importNode
                            |> Node.value
                            |> .exposingList
                            |> Maybe.map Node.value
                in
                case exposed of
                    Just (Exposing.Explicit importExposes) ->
                        importExposes
                            |> List.filterMap
                                (\(Node range expose) ->
                                    let
                                        imported =
                                            ( importedModule, expose |> exposeName )
                                    in
                                    if Set.member imported config.limited.specific then
                                        imported
                                            |> qualifiedToString
                                            |> Node range
                                            |> Just

                                    else
                                        Nothing
                                )

                    Just (Exposing.All range) ->
                        config.limited.specific
                            |> Set.filter
                                (\( module_, _ ) -> module_ == importedModule)
                            |> Set.toList
                            |> List.map
                                (\qualified ->
                                    qualified |> qualifiedToString |> Node range
                                )

                    Nothing ->
                        []
    in
    errors
        |> List.map
            (\name ->
                error { name = name, config = config }
            )


isStartOf : String -> String -> Bool
isStartOf module_ =
    \start ->
        module_ |> String.startsWith start


isEndingOf : String -> String -> Bool
isEndingOf module_ =
    \ending ->
        module_ |> String.endsWith ending


setAny : (element -> Bool) -> Set element -> Bool
setAny isOk =
    \set ->
        set
            |> Set.toList
            |> List.any isOk


expressionReview :
    { config : Config, context : ModuleContext }
    -> Node Expression
    -> List (Rule.Error {})
expressionReview { context, config } expressionNode =
    expressionNode
        |> expressionDirectNamedThings
        |> List.concatMap
            (namedReview { context = context, config = config })


declarationReview :
    { context : ModuleContext, config : Config }
    -> Declaration
    -> List (Rule.Error {})
declarationReview { context, config } =
    \declaration ->
        declaration
            |> declarationDirectNamedThings
            |> List.concatMap
                (namedReview { context = context, config = config })


namedReview :
    { context : ModuleContext
    , config : Config
    }
    -> Node String
    -> List (Rule.Error {})
namedReview { context, config } =
    \(Node range name) ->
        case range |> ModuleNameLookupTable.moduleNameAt context.moduleOriginLookup of
            -- declared in same module locally
            Just [] ->
                []

            Just (moduleNamePartsHead :: moduleNamePartsTail) ->
                let
                    module_ =
                        (moduleNamePartsHead :: moduleNamePartsTail)
                            |> moduleNameToString

                    reviewedModuleIsInsideLimitedScope =
                        moduleIsInsideLimitedScope
                            { limitedModule = module_
                            , limitedScope = config.limitedScope
                            , limitedStarts = config.limited.moduleStarts
                            , limitedEndings = config.limited.moduleEndings
                            }

                    qualified =
                        ( module_, name )
                in
                -- also checks module scope
                -- because an implicit import could be limited
                if qualified |> isLimited config.limited then
                    if context.moduleName |> reviewedModuleIsInsideLimitedScope then
                        []

                    else
                        [ error
                            { name = qualified |> qualifiedToString |> Node range
                            , config = config
                            }
                        ]

                else
                    []

            -- not expected
            Nothing ->
                [ Rule.error moduleNameLookupErrorInfo range ]



-- scope check


moduleIsInsideLimitedScope :
    { limitedModule : String
    , limitedScope : LimitedScope
    , limitedStarts : Set String
    , limitedEndings : Set String
    }
    -> String
    -> Bool
moduleIsInsideLimitedScope { limitedScope, limitedStarts, limitedEndings, limitedModule } =
    let
        isLimitedBaseModule baseModules_ =
            \module_ ->
                case baseModules_ of
                    Included ->
                        isOneOf
                            [ \() ->
                                limitedStarts
                                    |> setAny
                                        (\start ->
                                            (start ++ module_) == limitedModule
                                        )
                            , \() ->
                                limitedEndings
                                    |> setAny
                                        (\ending ->
                                            (module_ ++ ending) == limitedModule
                                        )
                            ]

                    NotIncluded ->
                        False

        moduleIsInside scope =
            \module_ ->
                isOneOf
                    [ \() -> module_ |> isLimitedBaseModule scope.baseModules
                    , \() -> module_ |> moduleIsInScope scope
                    ]
    in
    case limitedScope of
        Inside forbiddenScope ->
            moduleIsInside forbiddenScope

        OutsideOf allowedScope ->
            \module_ -> module_ |> moduleIsInside allowedScope |> not


isLimited : Limited -> ( String, String ) -> Bool
isLimited limited =
    \( module_, name ) ->
        isOneOf
            [ \() -> Set.member ( module_, name ) limited.specific
            , \() -> module_ |> moduleIsInScope limited
            ]


moduleIsInScope : ModuleScopeCommon scope_ -> String -> Bool
moduleIsInScope scope =
    \module_ ->
        isOneOf
            [ \() -> Set.member module_ scope.modules
            , \() ->
                scope.moduleStarts
                    |> setAny (\start -> module_ |> String.startsWith start)
            , \() ->
                scope.moduleEndings
                    |> setAny (\ending -> module_ |> String.endsWith ending)
            ]



-- error


moduleNameLookupErrorInfo : { message : String, details : List String }
moduleNameLookupErrorInfo =
    { message = "given `Range` not recognized by `ModuleNameLookupTable.moduleNameAt`"
    , details =
        [ "This bug originated from `elm-review`'s `ModuleNameLookupTable` or `elm-review-usage-scope-limit`"
        , "Please report it! Thanks <3"
        ]
    }


error :
    { name : Node String
    , config : Config
    }
    -> Rule.Error {}
error { name, config } =
    let
        nameBackQuoted =
            name |> Node.value |> backQuote
    in
    Rule.error
        { message =
            [ nameBackQuoted, " forbidden in this module" ]
                |> String.concat
        , details =
            ([ nameBackQuoted
             , " is "
             , config.limitedScope
                |> limitedScopeToString
                    { limitedStarts = config.limited.moduleStarts
                    , limitedEndings = config.limited.moduleEndings
                    }
             ]
                |> String.concat
            )
                :: config.reasonDetails
        }
        (name |> Node.range)


limitedScopeToString :
    { limitedStarts : Set String, limitedEndings : Set String }
    -> LimitedScope
    -> String
limitedScopeToString limitedExtensions =
    \limitedScope ->
        case limitedScope of
            Inside scope ->
                [ "only allowed in\n"
                , scope |> moduleScopeToString limitedExtensions
                ]
                    |> String.concat

            OutsideOf scope ->
                [ "forbidden in\n"
                , scope |> moduleScopeToString limitedExtensions
                ]
                    |> String.concat


moduleScopeToString :
    { limitedStarts : Set String, limitedEndings : Set String }
    -> ModuleScope
    -> String
moduleScopeToString { limitedStarts, limitedEndings } =
    \moduleScope ->
        [ Maybe.map (\starts -> "`module`s starting with " ++ setEnumerate starts)
            (setToFilled
                (moduleScope.moduleStarts
                    |> Set.map backQuote
                )
            )
        , Maybe.map (\endings -> "`module`s ending with " ++ setEnumerate endings)
            (setToFilled
                (moduleScope.moduleEndings
                    |> Set.map backQuote
                )
            )
        , Maybe.map (\modules_ -> "`module`s " ++ setEnumerate modules_)
            (setToFilled
                (moduleScope.modules
                    |> Set.map backQuote
                )
            )
        , case moduleScope.baseModules of
            NotIncluded ->
                Nothing

            Included ->
                [ "base `module`s `X` of "
                , [ limitedStarts
                        |> Set.toList
                        |> List.map (\start -> [ start, ".X" ] |> String.concat)
                  , limitedEndings
                        |> Set.toList
                        |> List.map (\ending -> [ "X.", ending ] |> String.concat)
                  ]
                    |> List.concat
                    |> List.map backQuote
                    |> String.join ", "
                ]
                    |> String.concat
                    |> Just
        ]
            |> List.filterMap identity
            |> List.map (\scoped -> [ "  - ", scoped ] |> String.concat)
            |> String.join "\n"
