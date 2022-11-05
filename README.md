# elm-review-usage-scope-limit

[`UsageScopeLimit`](https://package.elm-lang.org/packages/lue-bird/elm-review-usage-scope-limit/2.0.0/UsageScopeLimit): Limit using certain named things to certain spaces

> [The `Config` documentation](UsageScopeLimit#Config) covers **multiple examples and use-cases → Give them a 👀**

## improvements from [`NeoVier/elm-review-no-function-outside-of-modules`](https://dark.elm.dmy.fr/packages/NeoVier/elm-review-no-function-outside-of-modules/latest/), [`webbhuset/elm-review-forbid-specific-imports`](https://dark.elm.dmy.fr/packages/webbhuset/elm-review-forbid-specific-imports/latest/ForbidSpecificImports), [`kress95/elm-review-indirect-internal`](https://package.elm-lang.org/packages/kress95/elm-review-indirect-internal/latest/)

`elm-review-usage-scope-limit` covers the same functionality & more:
- limit named patterns, types, operators, functions, values
- limit for specific things or all members of packages or directories or modules – specific or with specific starts/endings
- limit to specific directories or modules or those with specific starts/endings
- `Config`s are more descriptive
- customize report details
- use [`Review.ModuleNameLookupTable`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-ModuleNameLookupTable)
    - cover implicit (default) imports
