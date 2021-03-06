# Fass - Fast SASS for Haskell [![Build Status](https://travis-ci.org/darthdeus/fass.svg?branch=master)](https://travis-ci.org/darthdeus/fass)

Fass is a SASS implementation for Haskell. The goal of the project is
to have complete compatibility with the [Ruby compiler](http://sass-lang.com).

~~**This project is still under heavy development and not ready for
  public use**~~
  
  **This project is no longer maintained.**

# Roadmap

While there are test in the project, the main test suite comes from
[sass-spec](https://github.com/sass/sass-spec). Currently the tests
are only running the basic suite, which contains most of the SASS
functionality.

Current progress is 14/59 features implemented.

## Feature list

- ~~Compile simple SCSS to CSS~~
- ~~Resolve nested rulesets in SCSS~~
- ~~Variables~~
- ~~Selector groups~~
- ~~Imports~~
- Hex arithmetic
- Mixins
- Functions
- Media queries
- Conditionals
- Percentages
- URLs

## Known issues

Imports are now implemented to resolve recursively, which works in
most cases, but can lead to an infinite loop when evaluating the
following example:

    // foo.scss
    @import "bar.scss";

    // bar.scss
    @import "foo.scss";

Basically any kind of circular imports will never terminate.

# Contributing

Right now the main goal is to get up to speed with the reference SASS
compiler. If you'd like to help out, the best thing you can do is pick
a specific feature from the list of failing specs and implement it.

The test suite under sass-spec comes from the
[sass-spec](https://github.com/sass/sass-spec) project. Thanks for
providing such a great test suite.

