{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse,
    Term (..),
  )
where

-- import qualified Data.Text as T
import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- true / false
-- if / then / else
-- fun x -> x
-- x
-- f x

data Term
  = BoolLiteral Bool
  | Var Text
  deriving (Show, Eq)

varRegex::Text
varRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

tokenize :: Text -> [Text]
tokenize program = getAllTextMatches (program =~ ("[a-zA-Z0-9_]+" :: Text))

parseExpression :: [Text] -> Either Text Term
parseExpression [] = Left "Unexpected end of program"
parseExpression (token : tokens) =
  case token of
    "true" -> Right $ BoolLiteral True
    "false" -> Right $ BoolLiteral False
    var | var =~ varRegex -> Right $ Var var
    other -> Left $ "Unexpected token: " <> other

parse :: Text -> Either Text Term
parse program = parseExpression $ tokenize program
