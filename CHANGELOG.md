# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- OTP 24 to CI targets
### Removed
- compatibility with OTP 19
- compatibility with OTP 20
- compatibility with OTP 21

## [1.2.1] - 2020-05-26
### Fixed
- outdated README

## [1.2.0] - 2020-05-26
### Removed
- compatibility with OTP 18

## [1.1.3] - 2019-11-11
### Changed
- generated documentation as to (tentatively) make it prettier

## [1.1.2] - 2019-09-21
### Fixed
- broken execution of test cases on OTP 22.1 when HiPE is available

## [1.1.1] - 2019-01-19
### Fixed
- unwarranted import of `rebar3_hex` plugin in library consumers

## [1.1.0] - 2018-11-25
### Added
- test coverage of release integration
- test coverage of static category configuration
- test coverage of static category configuration update
- test coverage of static category configuration dynamic override
- test coverage of dynamic category (re)configuration
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
