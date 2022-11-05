module Help exposing (alterCombined, areAll, backQuote, declarationDirectNamedThings, exposeName, expressionDirectNamedThings, isOneOf, moduleNameToString, packageOriginLookup, patternChildren, qualifiedToString, setEnumerate, setToFilled, typeChildren)

{-| Utility functions that would just clutter the rule's `module`
-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.TypeAnnotation as Type exposing (TypeAnnotation)
import Review.Project.Dependency
import Set exposing (Set)


{-| `||`/or chain in order.
-}
isOneOf : List (() -> Bool) -> Bool
isOneOf =
    List.any (\try -> try ())


{-| `&&`/and chain in order.
-}
areAll : List (() -> Bool) -> Bool
areAll =
    List.all (\try -> try ())


{-| Alter by a List of operations in order.
-}
alterCombined : List (value -> value) -> value -> value
alterCombined operations =
    \base ->
        operations
            |> List.foldl (\step -> step) base


backQuote : String -> String
backQuote =
    \code -> "`" ++ code ++ "`"


{-| Could make this return filled `Set`?
-}
setToFilled : Set comparableElement -> Maybe (Set comparableElement)
setToFilled =
    \set ->
        if set |> Set.isEmpty then
            Nothing

        else
            set |> Just


{-| Comma separated.
-}
setEnumerate : Set String -> String
setEnumerate =
    \set ->
        set |> Set.toList |> String.join ", "


moduleNameToString : Elm.Syntax.ModuleName.ModuleName -> String
moduleNameToString =
    String.join "."


qualifiedToString : ( String, String ) -> String
qualifiedToString =
    \( module_, name ) ->
        [ module_, ".", name ] |> String.concat


exposeName : TopLevelExpose -> String
exposeName =
    \topLevelExpose ->
        case topLevelExpose of
            Exposing.FunctionExpose name ->
                name

            Exposing.InfixExpose symbol ->
                symbol

            Exposing.TypeExpose { name } ->
                name

            Exposing.TypeOrAliasExpose name ->
                name


{-| All patterns that are itself part of the whole pattern.

Doesn't collect pattern literals, just looks one step deeper into the whole pattern.

-}
patternChildren : Pattern -> List (Node Pattern)
patternChildren =
    \pattern ->
        case pattern of
            Pattern.UnitPattern ->
                []

            Pattern.CharPattern _ ->
                []

            Pattern.IntPattern _ ->
                []

            Pattern.FloatPattern _ ->
                []

            Pattern.HexPattern _ ->
                []

            Pattern.StringPattern _ ->
                []

            Pattern.RecordPattern _ ->
                []

            Pattern.AllPattern ->
                []

            Pattern.VarPattern _ ->
                []

            Pattern.ParenthesizedPattern innerPattern ->
                [ innerPattern ]

            Pattern.AsPattern innerPattern _ ->
                [ innerPattern ]

            Pattern.UnConsPattern headPattern tailPattern ->
                [ headPattern, tailPattern ]

            Pattern.NamedPattern _ argumentPatterns ->
                argumentPatterns

            Pattern.TuplePattern partPatterns ->
                partPatterns

            Pattern.ListPattern elementPatterns ->
                elementPatterns


{-| All types that are itself part of the whole type.

One quirk: The extended type variable in a `GenericRecord` is regarded as a `GenericType` child.

Doesn't collect type literals, just looks one step deeper into the whole type.

-}
typeChildren : TypeAnnotation -> List (Node TypeAnnotation)
typeChildren =
    \type_ ->
        case type_ of
            Type.Unit ->
                []

            Type.GenericType _ ->
                []

            Type.FunctionTypeAnnotation argument result ->
                [ argument, result ]

            Type.Tupled parts ->
                parts

            Type.Typed _ arguments ->
                arguments

            Type.Record fields ->
                fields |> List.map (\(Node _ ( _, value )) -> value)

            Type.GenericRecord generic (Node _ fields) ->
                fields
                    |> List.map (\(Node _ ( _, value )) -> value)
                    |> (::) (generic |> Node.map Type.GenericType)



-- named


expressionDirectNamedThings : Node Expression -> List (Node String)
expressionDirectNamedThings =
    \(Node expressionRange expression) ->
        case expression of
            -- expression
            Expression.PrefixOperator symbol ->
                [ symbol |> Node expressionRange ]

            Expression.OperatorApplication symbol _ _ _ ->
                [ symbol |> Node expressionRange ]

            Expression.FunctionOrValue _ functionOrValueName ->
                [ functionOrValueName |> Node expressionRange ]

            Expression.RecordUpdateExpression recordNamed _ ->
                [ recordNamed ]

            -- pattern
            Expression.CaseExpression { cases } ->
                cases
                    |> List.concatMap
                        (\( casePattern, _ ) ->
                            casePattern |> patternsNamedCollect
                        )

            Expression.LambdaExpression { args } ->
                args |> List.concatMap patternsNamedCollect

            Expression.LetExpression { declarations } ->
                let
                    declarationLetNamedThings =
                        \(Node _ declarationLet) ->
                            case declarationLet of
                                Expression.LetFunction { declaration, signature } ->
                                    [ declaration
                                        |> Node.value
                                        |> .arguments
                                        |> List.concatMap patternsNamedCollect
                                    , case signature of
                                        Nothing ->
                                            []

                                        Just (Node _ { typeAnnotation }) ->
                                            typeAnnotation |> typesNamedCollect
                                    ]
                                        |> List.concat

                                Expression.LetDestructuring destructuringPattern _ ->
                                    destructuringPattern |> patternsNamedCollect
                in
                declarations
                    |> List.concatMap declarationLetNamedThings

            _ ->
                []


declarationDirectNamedThings : Declaration -> List (Node String)
declarationDirectNamedThings =
    \declaration ->
        case declaration of
            Declaration.FunctionDeclaration function ->
                [ case function.signature of
                    Just (Node _ { typeAnnotation }) ->
                        typeAnnotation |> typesNamedCollect

                    Nothing ->
                        []
                , function.declaration
                    |> Node.value
                    |> .arguments
                    |> List.concatMap patternsNamedCollect
                ]
                    |> List.concat

            Declaration.AliasDeclaration { typeAnnotation } ->
                typeAnnotation |> typesNamedCollect

            Declaration.CustomTypeDeclaration { constructors } ->
                constructors
                    |> List.concatMap
                        (\(Node _ { arguments }) ->
                            arguments
                                |> List.concatMap typesNamedCollect
                        )

            Declaration.PortDeclaration { typeAnnotation } ->
                typeAnnotation |> typesNamedCollect

            -- not possible in modules outside elm/...
            Declaration.InfixDeclaration { function } ->
                [ function ]

            -- Such a declaration doesn't exist in elm.
            Declaration.Destructuring _ _ ->
                []


patternsNamedCollect : Node Pattern -> List (Node String)
patternsNamedCollect =
    \(Node range pattern) ->
        [ case pattern of
            Pattern.NamedPattern { name } _ ->
                [ name |> Node range ]

            _ ->
                []
        , pattern
            |> patternChildren
            |> List.concatMap patternsNamedCollect
        ]
            |> List.concat


typesNamedCollect : Node TypeAnnotation -> List (Node String)
typesNamedCollect =
    \(Node range pattern) ->
        [ case pattern of
            Type.Typed nameMaybeQualified _ ->
                [ nameMaybeQualified
                    |> Node.map (\( _, name ) -> name)
                ]

            _ ->
                []
        , pattern
            |> typeChildren
            |> List.concatMap typesNamedCollect
        ]
            |> List.concat



--


packageOriginLookup :
    Dict String Review.Project.Dependency.Dependency
    -> Dict String { package : ( String, String ) }
packageOriginLookup =
    \dependencyProjects ->
        dependencyProjects
            |> Dict.toList
            |> List.concatMap
                (\( packageName, dependency ) ->
                    case packageName |> String.split "/" of
                        [ user, repo ] ->
                            dependency
                                |> Review.Project.Dependency.modules
                                |> List.map
                                    (\{ name } ->
                                        ( name, { package = ( user, repo ) } )
                                    )

                        -- not expected
                        _ ->
                            []
                )
            |> Dict.fromList
