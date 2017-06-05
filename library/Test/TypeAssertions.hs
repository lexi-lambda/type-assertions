{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides a set of runtime assertions about types that
-- propogates information back to the type system, using 'Typeable' and ':~:'.
-- These are intended to be used in a test suite (and exclusively in a test
-- suite) to create monomorphic implementations of polymorphic functions.
-- Specifically, this is intended to be used with a package like
-- <http://hackage.haskell.org/package/test-fixture test-fixture> to stub out
-- polymorphic typeclass methods with monomorphic implementations.
--
-- For example, consider the following typeclass:
--
-- > class Monad m => MonadDB m where
-- >   get :: DBRecord r => Id r -> m [r]
-- >   ...
--
-- The @get@ method might be used in another function to retrieve a record of a
-- specific type, such as a @User@. In a test, it might be useful to stub out
-- the @get@ method to provide a well-known user:
--
-- > let fixture = def { _get = \_ -> User { ... } }
--
-- However, this will not typecheck, since the type of record should be chosen
-- by the /caller/ of @get@. We might know that the type is guaranteed to be
-- @User@ in this particular case, but GHC cannot prove that, and it will reject
-- attempts to write a stubbed implementation.
--
-- To work around this issue, we can effectively defer the type error to runtime
-- by using the functions in this module. For example, we can use
-- 'withAssertEqT' to assert that the types are the same:
--
-- > {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- >
-- > let fixture = def { _get = \(_ :: Id r) ->
-- >       withAssertEqT @r @User $
-- >         User { ... } }
--
-- This will properly convince GHC that the @User@ value will only be returned
-- when the argument is actually an @Id User@. If it isn’t, 'withAssertEqT' will
-- throw.
module Test.TypeAssertions
  ( assertHasT
  , withAssertHasT
  , assertEqT
  , withAssertEqT
  , withAssertResultT
  , (:~:)(..)
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, (:~:)(Refl), eqT, typeRep)
import Data.Type.Equality (gcastWith)
import GHC.Stack (HasCallStack, withFrozenCallStack)

-- | Asserts that a value has a particular type, and raises an exception if it
-- does not.
--
-- >>> assertHasT @Bool True
-- Refl
-- >>> assertHasT @Bool "hello"
-- *** Exception: expected value of type ‘Bool’, but got ‘"hello"’, which is of type ‘[Char]’
assertHasT :: forall b a. (HasCallStack, Show a, Typeable a, Typeable b) => a -> (a :~: b)
assertHasT x = withFrozenCallStack $ fromMaybe (error errorMessage) eqT
  where errorMessage =
            "expected value of type ‘" <> show (typeRep (Proxy :: Proxy b))
         <> "’, but got ‘" <> show x
         <> "’, which is of type ‘" <> show (typeRep (Proxy :: Proxy a)) <> "’"

-- | Like 'assertHasT', but instead of returning a witness upon success,
-- 'withAssertHasT' returns its second argument. The second argument can be an
-- expression that typechecks under the assumption that @a ~ b@.
--
-- > foo :: (Show a, Typeable a) => a -> a
-- > foo x = withAssertHasT @String x $ map toUpper x
--
-- >>> foo "hello"
-- "HELLO"
-- >>> foo True
-- *** Exception: expected value of type ‘[Char]’, but got ‘True’, which is of type ‘Bool’
-- CallStack (from HasCallStack):
--   withAssertHasT, called at <interactive>:17:13 in interactive:Ghci1
withAssertHasT :: forall b a c. (HasCallStack, Show a, Typeable a, Typeable b) => a -> ((a ~ b) => c) -> c
withAssertHasT x = withFrozenCallStack $ gcastWith (assertHasT x :: a :~: b)

-- | Like 'assertHasT', but asserts that two types are the same instead of
-- asserting that a value has a type. Generally, prefer 'assertHasT' when
-- possible, since it will produce better error messages, but 'assertEqT' can be
-- necessary when the type does not have a runtime representation (such as if it
-- is phantom).
--
-- >>> assertEqT @Bool @Bool
-- Refl
-- >>> assertEqT @Bool @Int
-- *** Exception: expected type ‘Int’, but got type ‘Bool’
assertEqT :: forall a b. (HasCallStack, Typeable a, Typeable b) => (a :~: b)
assertEqT = withFrozenCallStack $ fromMaybe (error errorMessage) eqT
  where errorMessage =
            "expected type ‘" <> show (typeRep (Proxy :: Proxy b))
         <> "’, but got type ‘" <> show (typeRep (Proxy :: Proxy a)) <> "’"

-- | Like 'assertEqT' but with the type propogation behavior of
-- 'withAssertHasT'. Generally, prefer 'withAssertHasT' when possible, since it
-- will produce better error messages, but 'withAssertEqT' can be necessary when
-- the type does not have a runtime representation (such as if it is phantom).
--
-- > foo :: forall a proxy. (Show a, Typeable a) => proxy a -> a
-- > foo _ = withAssertEqT @a @Bool True
--
-- >>> foo (Proxy @Bool)
-- True
-- >>> foo (Proxy @Int)
-- *** Exception: expected type ‘Bool’, but got type ‘Int’
withAssertEqT :: forall a b c. (HasCallStack, Typeable a, Typeable b) => ((a ~ b) => c) -> c
withAssertEqT x = withFrozenCallStack $ gcastWith (assertEqT :: a :~: b) x

-- | Like 'withAssertEqT' but asserts the result type of the overall expression
-- rather than some arbitrary type. This is useful when implementing a function
-- polymorphic in the return type where the result type does not depend on the
-- type of an argument.
--
-- > foo :: Typeable a => String -> a
-- > foo _ = withAssertResultT True
--
-- >>> foo "hello" :: Bool
-- True
-- >>> foo "hello" :: Int
-- *** Exception: expected type ‘Bool’, but got type ‘Int’
withAssertResultT :: forall a b. (HasCallStack, Typeable a, Typeable b) => a -> b
withAssertResultT x = withFrozenCallStack $ gcastWith (assertEqT :: b :~: a) x
