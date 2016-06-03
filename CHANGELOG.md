# 0.3.2.0

- Add `Data.Aeson.Extra.Stream` with `streamDecode`

# 0.3.1.1 (2016-02-09)

- Support aeson-0.11

# 0.3.1.0 (2015-12-27)

- Add `Data.Aeson.Extra.TH`
- Add `Data.Aeson.Extra.Foldable`
- Add `Data.Aeson.Extra.Merge`

# 0.3.0.1 (2016-01-26)

- Support `quickcheck-instances >=0.3.12`

# 0.3.0.0 (2015-12-25)

- `Data.Time.TH` moved to [`time-parsers`](http://hackage.haskell.org/package/time-parsers)
- `Data.Aeson.Compat` moved to [`aeson-compat`](http://hackage.haskell.org/package/aeson-compat)
- The modules aren't re-exported, as that would require `Cabal >= 1.21` restriction

# 0.2.3.0 (2015-12-08)

- `Data.Time.TH` module with `mkUTCTime`
- `encodeStrict` in `Data.Aeson.Extra`

# 0.2.2.0 (2015-11-10)

- `U` and `Z` to parse `UTCTime` and `ZonedTime` compatibly
- Orphans `FromJSON` for `Day` and `LocalTime`

# 0.2.1.0 (2015-10-05) GHC 7.6 Support

- No `SymTag` or `SingObject` support

# 0.2.0.0 (2015-09-29) No ListLike

- Make `CollapsedList` use typeclasses in `base`

# 0.1.0.0 (2015-09-29) Initial release
