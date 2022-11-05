module UsageScopeLimit.Test exposing (tests)

{-| TODO
-}

import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import UsageScopeLimit


tests : Test
tests =
    describe "UsageScopeLimit"
        [ specific, base ]


base : Test
base =
    let
        baseRule =
            UsageScopeLimit.for
                [ UsageScopeLimit.modulesEndingWith [ "Internal" ] ]
                |> UsageScopeLimit.toInside
                    [ UsageScopeLimit.baseModules ]
                |> UsageScopeLimit.because [ "consistency" ]
    in
    describe "base"
        [ describe "reported"
            [ test "imported outside of base module"
                (\() ->
                    """module Structure exposing (main)

import Structure.Element.Internal

a = a
"""
                        |> Review.Test.run baseRule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "`Structure.Element.Internal` forbidden in this module"
                                , details = [ "" ]
                                , under = "Structure.Element.Internal"
                                }
                            ]
                )
            ]
        , describe "accepted"
            [ test "imported outside of base module"
                (\() ->
                    """module Structure exposing (main)

import Structure.Internal

a = a
"""
                        |> Review.Test.run baseRule
                        |> Review.Test.expectNoErrors
                )
            ]
        ]


specific : Test
specific =
    let
        specificConfig =
            UsageScopeLimit.for
                [ UsageScopeLimit.specific
                    [ ( "Html", "input" ), ( "Html", "textarea" ) ]
                ]
                |> UsageScopeLimit.toInside
                    [ UsageScopeLimit.modules [ "View.Input" ] ]
                |> UsageScopeLimit.because [ "consistency" ]
    in
    describe "specific"
        [ describe "reported"
            [ test "using Html.input and exposed Html.textarea outside of module"
                (\() ->
                    """module Main exposing (main)

import Html exposing (textarea)

main =
    Html.div []
        [ Html.input [] []
        , textarea [] []
        ]
"""
                        |> Review.Test.run specificConfig
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "`Html.input` forbidden in this module"
                                , details =
                                    [ "The `Html.input` function is only allowed to be used in these modules:\n  - `module`s `View.Input`"
                                    , "consistency"
                                    ]
                                , under = "Html.input"
                                }
                            , Review.Test.error
                                { message = "`Html.textarea` forbidden in this module"
                                , details =
                                    [ "The `Html.textarea` function is only allowed to be used in these modules:\n  - `module`s `View.Input`"
                                    , "consistency"
                                    ]
                                , under = "textarea"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 23 }
                                    , end = { row = 3, column = 30 }
                                    }
                            ]
                )
            ]
        , describe "accepted"
            [ test "specific inside of module"
                (\() ->
                    """module View.Input exposing (main)

import Html

main =
    Html.div []
        [ Html.input [] []
        , Html.textarea [] []
        ]
"""
                        |> Review.Test.run specificConfig
                        |> Review.Test.expectNoErrors
                )
            ]
        ]
