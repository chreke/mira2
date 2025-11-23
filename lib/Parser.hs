{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse,
    Term (..),
  )
where

-- import qualified Data.Text as T

import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- TODO:
-- [x] true / false
-- [ ] if / then / else
-- [ ] fun x -> x
-- [x] x
-- [ ] f x

type Parser a = ExceptT Text (State [Text]) a

data Term
  = BoolLiteral Bool
  | Var Text
  deriving (Show, Eq)

varRegex :: Text
varRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

tokenize :: Text -> [Text]
tokenize program = getAllTextMatches (program =~ ("[a-zA-Z0-9_]+" :: Text))

nextToken :: Parser (Maybe Text)
nextToken = do
  tokens <- get
  case tokens of
    [] -> return Nothing
    (t : rest) -> do
      put rest
      return $ Just t

parseExpression :: Parser Term
parseExpression = do
  token <- nextToken
  case token of
    Nothing -> throwError "Unexpected end of program"
    Just "true" -> return $ BoolLiteral True
    Just "false" -> return $ BoolLiteral False
    Just var | var =~ varRegex -> return $ Var var
    Just other -> throwError $ "Unexpected token: " <> other

parse :: Text -> Either Text Term
parse program =
  let tokens = tokenize program
   in evalState (runExceptT parseExpression) tokens
