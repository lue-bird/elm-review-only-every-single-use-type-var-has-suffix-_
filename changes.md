# changelog

## 2.0.0

- fixed a bug in NoMultiUseTypeVarsEndWith_'s fix that removed the last symbol of every type variable in that type
- As [jfmengels](https://github.com/jfmengels) pointed out, the rules in `NoMultiUseTypeVarsEndWith_` & `SingleUseTypeVarsEndWith_` should always be used together. The rule in `OnlyAllSingleUseTypeVarsEndWith_` now does both jobs

#### 2.0.1

- phantom types in `type`s aren't reported anymore
