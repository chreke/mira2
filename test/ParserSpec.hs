{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Parser (Term (..), parse)
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses a boolean literal" $ do
      parse "true" `shouldBe` Right (BoolLiteral True)
      parse "false" `shouldBe` Right (BoolLiteral False)
    it "parses a variable" $ do
      parse "x" `shouldBe` Right (Var "x")

-- it "parses an empty string" $ do
--   parse "" `shouldBe` ()
--
-- it "parses a simple string" $ do
--   parse "hello" `shouldBe` ()
--
-- it "parses a multi-line string" $ do
--   parse "line1\nline2\nline3" `shouldBe` ()
--
-- it "parses a string with special characters" $ do
--   parse "!@#$%^&*()" `shouldBe` ()
