# type-assertions [![Build Status](https://travis-ci.org/lexi-lambda/type-assertions.svg?branch=master)](https://travis-ci.org/lexi-lambda/type-assertions)

This module provides a set of runtime assertions about types that propogates information back to the type system, using `Data.Typeable` and `Data.Type.Equality`. These assertions are intended to be used in a test suite (and exclusively in a test suite) to create monomorphic implementations of polymorphic functions. Specifically, this is intended to be used with a package like [test-fixture][] to stub out polymorphic typeclass methods with monomorphic implementations.

For more information, [see the documentation on Hackage][type-assertions].

[test-fixture]: http://hackage.haskell.org/package/test-fixture
[type-assertions]: http://hackage.haskell.org/package/type-assertions
