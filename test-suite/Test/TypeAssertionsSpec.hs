{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.TypeAssertionsSpec (spec) where

import Test.Hspec
import Test.TypeAssertions

import Control.Exception (evaluate)
import Data.Typeable (Typeable)

import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

newtype Id a = Id Int
  deriving (Eq, Show, Num)

class (Show a, Typeable a) => DBRecord a

data User = User
  deriving (Eq, Show)
instance DBRecord User

data Post = Post
  deriving (Eq, Show)
instance DBRecord Post

class Monad m => DB m where
  fetchRecord :: DBRecord a => Id a -> m (Maybe a)
  insertRecord :: DBRecord a => a -> m (Maybe (Id a))

mkFixture "Fixture" [ts| DB |]

spec :: Spec
spec = do
  describe "assertEq" $ do
    let fixture :: FixturePure = def {
          _insertRecord = \record -> case assertHasT @Post record of
            Refl -> return $ Just 3 }

    it "produces a witness if the provided value matches the given type" $ do
      let result = unTestFixture (insertRecord Post) fixture
      result `shouldBe` Just 3

    it "throws an exception if the provided value does not match the given type" $ do
      let result = unTestFixture (insertRecord User) fixture
      let message = "expected value of type ‘Post’, but got ‘User’, which is of type ‘User’"
      evaluate result `shouldThrow` errorCall message

  describe "withAssertEq" $ do
    let fixture :: FixturePure = def {
          _insertRecord = \record -> withAssertHasT @Post record $
            return $ Just 3 }

    it "produces a witness if the provided value matches the given type" $ do
      let result = unTestFixture (insertRecord Post) fixture
      result `shouldBe` Just 3

    it "throws an exception if the provided value does not match the given type" $ do
      let result = unTestFixture (insertRecord User) fixture
      let message = "expected value of type ‘Post’, but got ‘User’, which is of type ‘User’"
      evaluate result `shouldThrow` errorCall message

  describe "assertEqT" $ do
    let fixture :: FixturePure = def {
          _fetchRecord = \(_ :: Id record) -> case assertEqT @record @Post of
            Refl -> return $ Just Post }

    it "gains type information when the given types match" $ do
      let result = unTestFixture (fetchRecord 0) fixture
      result `shouldBe` Just Post

    it "throws an exception if the given types do not match" $ do
      let result = unTestFixture (fetchRecord (0 :: Id User)) fixture
      let message = "expected type ‘Post’, but got type ‘User’"
      evaluate result `shouldThrow` errorCall message

  describe "withAssertEqT" $ do
    let fixture :: FixturePure = def {
          _fetchRecord = \(_ :: Id record) -> withAssertEqT @record @Post $
            return $ Just Post }

    it "gains type information when the given types match" $ do
      let result = unTestFixture (fetchRecord 0) fixture
      result `shouldBe` Just Post

    it "throws an exception if the given types do not match" $ do
      let result = unTestFixture (fetchRecord (0 :: Id User)) fixture
      let message = "expected type ‘Post’, but got type ‘User’"
      evaluate result `shouldThrow` errorCall message
