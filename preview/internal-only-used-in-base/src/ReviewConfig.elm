module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

Rules on radar

  - [`jfmengels/elm-review-cognitive-complexity`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-cognitive-complexity/latest/CognitiveComplexity)
      - I currently fear that there will be
        exceptions where we can't
        abstract/simplify functions further

-}

import Docs.NoMissing
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoforbiddenWords
import NoFunctionOutsideOfModules
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoPrimitiveTypeAlias
import NoRecordAliasConstructor
import NoSinglePatternCase
import NoUnsortedLetDeclarations
import NoUnsortedTopLevelDeclarations
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import OnlyAllSingleUseTypeVarsEndWith_
import Review.Rule as Rule exposing (Rule)
import ReviewPipelineStyles as Pipeline
import ReviewPipelineStyles.Fixes as PipelineFix
import ReviewPipelineStyles.Predicates as PipelinePredicate
import ReviewPipelineStyles.Premade as Pipeline
import Simplify
import NoAlways


config : List Rule
config =
    [ -- ## documentation
      Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }

    -- ## simplify
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , NoBooleanCase.rule
    , NoSinglePatternCase.rule
        (NoSinglePatternCase.fixInArgument
            |> NoSinglePatternCase.ifAsPatternRequired
                (NoSinglePatternCase.fixInLetInstead
                    |> NoSinglePatternCase.andIfNoLetExists
                        NoSinglePatternCase.createNewLet
                )
        )

    -- ## limit
    , [ [ [ Pipeline.parentheticalApplicationPipelines
                |> Pipeline.limit
                |> Pipeline.that
                    (PipelinePredicate.haveAnyNonInputStepThatIs
                        PipelinePredicate.aSemanticallyInfixFunction
                    )
                |> Pipeline.andTryToFixThemBy PipelineFix.convertingToRightPizza
                |> Pipeline.andCallThem "parenthetical application of a semantically-infix function"
          ]
        , [ Pipeline.rightPizzaPipelines
                |> Pipeline.limit
                |> Pipeline.that
                    (PipelinePredicate.haveAnyStepThatIs
                        PipelinePredicate.aConfusingNonCommutativeFunction
                    )
                |> Pipeline.andCallThem
                    "|> pipeline with confusing non-commutative function"
          , Pipeline.parentheticalApplicationPipelines
                |> Pipeline.limit
                |> Pipeline.that
                    (PipelinePredicate.haveAnyStepThatIs
                        PipelinePredicate.aConfusingNonCommutativeStartOperator
                    )
                |> Pipeline.andCallThem
                    "parenthetical application with confusing non-commutative start operator"
          ]
        ]
            |> List.concat
      , [ Pipeline.leftPizzaPipelines
            |> Pipeline.limit
            |> Pipeline.andTryToFixThemBy
                PipelineFix.convertingToParentheticalApplication
            |> Pipeline.andReportCustomError
                "<| pipeline"
                [ "forbidding `f <| a s` for reasons of simplicity, consistency:"
                , "  - Pipe data before the function: `food |> op ...`"
                , "  - Feed arguments after the function: `... |> opWith (a ...) (b ...)`"
                , "Use the application style `f (a s)` instead"
                ]
        , Pipeline.leftCompositionPipelines
            |> Pipeline.limit
            |> Pipeline.andReportCustomError
                "<< pipeline"
                [ "forbidding `g << f` for reasons of simplicity, readability, consistency:"
                , "  - Keep the order data comes from before and gets piped through functions after: `... |> opF |> opG`"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
        , Pipeline.rightCompositionPipelines
            |> Pipeline.limit
            |> Pipeline.andReportCustomError
                ">> pipeline"
                [ "forbidding `g >> f` for reasons of simplicity, consistency:"
                , [ "Establish a subject: `List.map (\\user -> user |> User.badgeAdd ... |> User.levelIncrease)`"
                  , " for easier readability and scalability (maybe even creating a separate function)"
                  , " when chaining multiple operations"
                  ]
                    |> String.concat
                ]
        ]
      ]
        |> List.concat
        |> Pipeline.rule
    , NoPrimitiveTypeAlias.rule
    , OnlyAllSingleUseTypeVarsEndWith_.rule
    , NoRecordAliasConstructor.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoforbiddenWords.rule forbiddenWords
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoUnsortedTopLevelDeclarations.rule
        (NoUnsortedTopLevelDeclarations.sortTopLevelDeclarations
            |> NoUnsortedTopLevelDeclarations.glueHelpersAfter
            |> NoUnsortedTopLevelDeclarations.glueDependenciesBeforeFirstDependent
        )
    , NoUnsortedLetDeclarations.rule
        (NoUnsortedLetDeclarations.sortLetDeclarations
            |> NoUnsortedLetDeclarations.glueDependenciesBeforeFirstDependent
        )
    , NoFunctionOutsideOfModules.rule
        [ ( forbiddenFunctionOrValues, [] ) ]
    , NoAlways.rule
    , -- could be included in the above
      NoDebug.Log.rule
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

    -- use a `Set`, `Dict` or `List.sortWith`
    , "List.sort"
    , "List.sortBy"
    ]


forbiddenWords : List String
forbiddenWords =
    [ [ "REPLACEME", "replaceme", "replace-me", "ReplaceMe" ]
    , [ "ToReplace", "TOREPLACE", "to-replace" ]
    , [ "TODO", "todo", "Todo", "to-do", "ToDo" ]
    , [ "- []" ]
    , [ "ToCheck", "to-check" ]
    , [ "ToFix", "TOFIX" ]
    , [ "FIXME", "fixme", "FixMe", "Fixme" ]
    ]
        |> List.concat
