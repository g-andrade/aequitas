# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [UNRELEASED]
### Added
- test coverage of release integration
### Changed
- format of static category configuration (no API breakage as the old format was incompatible with releases in any case)
### Fixed
- compatibility of static category configuration with releases [thanks to leoliu for reporting the issue]

## [1.0.1] - 2018-06-12
### Fixed
- building when HiPE is not supported [thanks to Rui Coelho for reporting this]

## [1.0.0] - 2018-05-06
### Added
- outlier detection using IQR
- collective rate limiting
