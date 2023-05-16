# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]



## [0.7.0]

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



## [0.6.0] - 2023-06-16

## [0.5.0] - 2022-05-19

## 0.4.0 - 2022-03-01

## 0.3.0 - 2021-12-08




<!-- Unreleased -->
[#44]: https://github.com/ConnorGray/Organizer/pull/44

<!-- This needs to be updated for each tagged release. -->
[Unreleased]: https://github.com/ConnorGray/Organizer/compare/v0.6.0...HEAD

[0.6.0]: https://github.com/ConnorGray/Organizer/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/ConnorGray/Organizer/compare/v0.4.0...v0.5.0