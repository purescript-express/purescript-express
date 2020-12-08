# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

- (#104): type for Middleware was changed from `Fn3 Request Response (Effect Unit) (Effect Unit)` to `EffectFn3 Request Response (Effect Unit) Unit`

New features:

- (#104): the `Middleware` type alias was added

Bugfixes:

Other improvements:

## v0.8.0

- The first version with CHANGELOG.md
