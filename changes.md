# changelog

#### 2.0.4

  - remove dependency `elm-community/list-extra`
  - remove dependency `turboMaCk/non-empty-list-alias`
  - fix bug where type variables that are keywords suffixed with underscore were reported as having unnecessary underscores
  - faster

#### 2.0.3

  - `elm-review` → >= 2.10.0
      - `providesFixes...` add

#### 2.0.2

  - removed fixes for let declaration types because -\_ suffixed type variables could come from the top-level declaration type

#### 2.0.1

  - phantom types in `type`s aren't reported anymore

## 2.0.0

  - fixed a bug in NoMultiUseTypeVarsEndWith_'s fix that removed the last symbol of every type variable in that type
  - as [jfmengels](https://github.com/jfmengels) pointed out, the rules in `NoMultiUseTypeVarsEndWith_` & `SingleUseTypeVarsEndWith_` should always be used together. The rule in `OnlyAllSingleUseTypeVarsEndWith_` now does both jobs
