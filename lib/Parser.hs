{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse,
    Term (..),
  )
where

-- import qualified Data.Text as T

import Control.Monad.State
import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- TODO:
-- [x] true / false
-- [ ] if / then / else
-- [ ] fun x -> x
-- [x] x
-- [ ] f x

type ParserState a = State [Text] a

-- nextToken :: ParserState (Maybe Text)
-- nextToken = do
--   tokens <- get
--   case tokens of
--     [] -> return Nothing
--     token : tokens' -> do
--         put tokens'
--         return token
--
--
data Term
  = BoolLiteral Bool
  | Var Text
  deriving (Show, Eq)

varRegex :: Text
varRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

tokenize :: Text -> [Text]
tokenize program = getAllTextMatches (program =~ ("[a-zA-Z0-9_]+" :: Text))

nextToken :: ParserState (Maybe Text)
nextToken = do
  tokens <- get
  case tokens of
    [] -> return Nothing
    (t : rest) -> do
      put rest
      return $ Just t

parseExpression :: ParserState (Either Text Term)
parseExpression = do
  token <- nextToken
  case token of
    Just "true" -> return $ Right $ BoolLiteral True
    Just "false" -> return $ Right $ BoolLiteral False
    Just var | var =~ varRegex -> return $ Right $ Var var
    Just other -> return $ Left $ "Unexpected token: " <> other
    Nothing -> return $ Left "Unexpected end of program"

parse :: Text -> Either Text Term
parse program =
  let tokens = tokenize program
   in evalState parseExpression tokens
