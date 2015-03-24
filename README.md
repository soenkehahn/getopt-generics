# args-generics

## status

Early draft.

## todos

- get rid of cabal dependency safe
- implement support for short options
  - We should have a pure core function like this:
    `:: (AllTheConstraints a) => [String] -> SomeConfigurationType -> Either String a`
    or something like this. The configuration type would be used to configure
    short options (but also other things, see below). Then we could have
    `:: (...) => Proxy a -> SomeConfigurationType`
    to automatically come up with nice short options. `withArguments` would then
    be implemented on top of these two functions.
- implement automatic creation of short options
- implement renaming of fields/options (e.g. map `mySettingsTypePort` to `port`)
  using `SomeConfigurationType` from above.
- implement commands (with sum types)
