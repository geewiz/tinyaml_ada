# Changelog

All notable changes to this project will be documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

> [!NOTE]
> Until the release of version 1.0, this project will be in development
> mode. In consequence, even minor versions might contain breaking changes.
> Starting with version 1.0, we'll follow [Semantic
> Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- Instead of a homegrown test suite, we now use AUnit.

## [0.2.0] - 2025-12-29

### Changed

- The library was missing memory management. This release introduces the
 Controlled Type `Document` that'll trigger a cleanup when it goes out of scope.
 See [README](README.md) and the example programs for how this works.

## [0.1.0] - 2025-12-24

### Added

- First release of the library. It's going to be in development mode until
  release 1.0. The practical consequence is that until then, even minor versions
  might well contain _breaking changes_.
