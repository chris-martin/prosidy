# v1.6.0.1 _(2020-03-13)_
- Added patterns to `Series` and `SeriesNE` to make for easier pattern
  matching.
- Added the `BlockRegion`, `InlineRegion`, and `LiteralRegion` synonyms.

# v1.6 _(2020-03-01)_
- Removed an erroneous `Monoid` constraint from `SeriesNE`
- Added locations to a few node types.
- Wrapped 'Text' nodes in 'Fragment'.

# v1.5.0.1 _(2020-02-24)_
- Added `CHANGELOG` to its own file.
- Included `CHANGELOG` and `README.pro` in the source distribution.
- Added an optic for fetching the name of a `Tag`.

# v1.5.0.0 _(2020-02-23)_
- Added tests for GHC 8.4.4 through 8.6.1.
- Initial stable release.