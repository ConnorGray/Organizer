# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]


## [0.7.2] — 2024-03-08

### Added

* Added new numbered item TODO cells. Typing `[` as the first character in an
  `"ItemNumbered"` cell will transform it into a `"TODO:ItemNumbered"` cell. ([#57])

* Add new `.` style key mapping. Typing `.` as the first character in a `"TODO"`
  or `"TODO:Item"` cell will transform it into a `"TODO:ItemNumbered"` cell. ([#57])

* Added support for a new 'Note' notebook type. ([#60])

### Changed

* Reposition TODO cell checkbox to stay vertically aligned against the top
  edge of the cell, instead of being vertically centered. ([#56], [#61], [#62])

  This makes the TODO checkbox visually behave like the "▪"︎ bullet shown in Item
  cells. 🎉

* Inline the `Workspace ...` switching buttons into the command palette dropdown
  to avoid extra unnecessary clicks. ([#58])

* 20x speedup in report generation by avoiding expensive `NotebookWrite`
  operations. What previously could take 10-20s now takes <1 second. ([#59])

* Avoid showing "incomplete" new notebooks that are still under construction to
  the user. This is analogous to the visual effect of an "unstyled flash"
  problem of web technologies. ([#63])

* Improved Web Page and Email links to use "Text" styling instead of default
  monospaced box formatting.

### Fixed

* Added workaround for encoding issue that would cause junk characters to appear
  in Web Page and Email links that contained a "•" or "—" character. ([#64])



## [0.7.1] — 2023-09-06

### Added

* Added new `File > New > Organizer Notebook` menu with `Design Notebook` and
  `Tasklist Notebook` items.

  This makes it easier to use Organizer's useful non-Log notebook types without
  using the full Organizer directory structure.

### Changed

* Replace custom error handling with new dependency on
  [Wolfram/ErrorTools](https://paclets.com/Wolfram/ErrorTools).



## [0.7.0] — 2023-05-16

### Added

* Added new Organizer.nb stylesheet, making it easier to use new Organizer UI
  features in older organizer notebooks, and ensuring that new UI features are
  available automatically when the Organizer paclet is updated.

  Notebooks created with previous versions of Organizer can be updated to use
  the new Organizer stylesheet by selecting the
  `Format > Stylesheet... > ConnorGray > Organizer` menu item. This will clear
  the old Organizer styles that are inlined into every notebook created by
  older versions of Organizer.

* Add new `/` style key mapping for a new cell insertion menu.

  This functionality is supported by a dependency on the
  [ConnorGray/CellInsertionMenu](https://paclets.com/ConnorGray/CellInsertionMenu)
  paclet.

* Add new `[` style key mapping to conveniently insert a new TODO cell.

### Fixed

- Fixed parsing of double quote characters in 'Web Link' and 'Email' content
  returned from AppleScript. ([#44])



## [0.6.0] - 2023-02-16

## [0.5.0] - 2022-05-19

## 0.4.0 - 2022-03-01

## 0.3.0 - 2021-12-08


<!-- v0.7.0 -->
[#44]: https://github.com/ConnorGray/Organizer/pull/44
[#47]: https://github.com/ConnorGray/Organizer/pull/47

<!-- v0.7.1 -->
[#55]: https://github.com/ConnorGray/Organizer/pull/55

<!-- v0.7.2 -->
[#56]: https://github.com/ConnorGray/Organizer/pull/56
[#57]: https://github.com/ConnorGray/Organizer/pull/57
[#58]: https://github.com/ConnorGray/Organizer/pull/58
[#59]: https://github.com/ConnorGray/Organizer/pull/59
[#60]: https://github.com/ConnorGray/Organizer/pull/60
[#61]: https://github.com/ConnorGray/Organizer/pull/61
[#62]: https://github.com/ConnorGray/Organizer/pull/62
[#63]: https://github.com/ConnorGray/Organizer/pull/63
[#64]: https://github.com/ConnorGray/Organizer/pull/64

<!-- Unreleased -->

<!-- This needs to be updated for each tagged release. -->
[Unreleased]: https://github.com/ConnorGray/Organizer/compare/v0.7.2...HEAD

[0.7.2]: https://github.com/ConnorGray/Organizer/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/ConnorGray/Organizer/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/ConnorGray/Organizer/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/ConnorGray/Organizer/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/ConnorGray/Organizer/compare/v0.4.0...v0.5.0
