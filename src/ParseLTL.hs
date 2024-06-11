{-# LANGUAGE OverloadedStrings #-}

module ParseLTL (Formula(..), parseLTL) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parseTest, empty, between, many, (<?>), eof, choice, noneOf, some, try, (<|>), runParser, parseMaybe)
import Text.Megaparsec.Char (space1, alphaNumChar, string)
import Text.Megaparsec.Char.Lexer (space, symbol, skipLineComment, decimal)
import Text.Megaparsec.Stream (Token)
import Data.Text (Text, cons)
import qualified Data.Text.IO as TextIO
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Control.Monad (void)

type Predicate = ByteString
data Constraint = AtEnd Predicate
                | Always Predicate
                | Sometime Predicate
                | Within Int Predicate
                | AtMostOnce Predicate
                | SometimeAfter Predicate Predicate
                | SometimeBefore Predicate Predicate
                | AlwaysWithin Int Predicate Predicate
                | HoldDuring Int Int Predicate
                | HoldAfter Int Predicate
                deriving Show

data Constraint' = Hard Constraint
                 | Soft Constraint
                 deriving Show

type Constraints = [Constraint']

type Input = Text
type Parser = Parsec Void Input

pSpace :: Parser ()
pSpace = space space1 (skipLineComment ";") empty <?> "space or comment"

pSymbol :: Input -> Parser Input
pSymbol = symbol pSpace

parens :: Parser a -> Parser a
parens p = between (pSymbol "(") (pSymbol ")") p <?> "parentheses"

someBut :: [Token Input] -> Parser [Token Input]
someBut l = some $ noneOf l

parsePredicate :: Parser Predicate
parsePredicate = parens (C8.pack <$> someBut "()" <?> "predicate") <* pSpace

pKey :: Input -> Parser Input
pKey keyword = string keyword <* pSpace

parseConstraint :: Parser Constraint
parseConstraint = parens (choice
  [ AtEnd <$> (pKey "at end" *> parsePredicate),
    Within <$> (pKey "within" *> decimal) <*> parsePredicate,
    AtMostOnce <$> (pKey "at-most-once" *> parsePredicate),
    SometimeAfter <$> (pKey "sometime-after" *> parsePredicate) <*> parsePredicate <?> "sometime-after",
    SometimeBefore <$> (pKey "sometime-before" *> parsePredicate) <*> parsePredicate <?> "sometime-before",
    Sometime <$> (pKey "sometime" *> parsePredicate),
    AlwaysWithin <$> (pKey "always-within" *> decimal) <*> parsePredicate <*> parsePredicate,
    Always <$> (pKey "always" *> parsePredicate),
    HoldDuring <$> (pKey "hold-during" *> decimal) <*> decimal <*> parsePredicate,
    HoldAfter <$> (pKey "hold-after" *> decimal) <*> parsePredicate ] <?> "constraint")

parseSoftConstraint :: Parser Constraint'
parseSoftConstraint = Soft <$> (pKey "preference" *> some alphaNumChar *> pSpace *> parseConstraint)

parseHardConstraint :: Parser Constraint'
parseHardConstraint = Hard <$> parseConstraint

parseConstraint' :: Parser Constraint'
parseConstraint' = parens (parseSoftConstraint <|> parseHardConstraint)

skipBlock :: Parser ()
skipBlock = void $ parens $ many (void (someBut "()") <|> skipBlock <?> "skipping this part")

parseBlock :: Input -> Parser a -> Parser a
parseBlock keyword pInner = parens (string keyword *> pSpace *> pInner <?> show keyword)

parseAndBlock :: Parser [Constraint']
parseAndBlock = parseBlock "and" (many parseConstraint')

singleConstraint :: Parser [Constraint']
singleConstraint = (:[]) <$> parseConstraint'

parseConstraintsBlock :: Parser [Constraint']
parseConstraintsBlock = parseBlock ":constraints" $ try parseAndBlock <|> singleConstraint

parseDefineBlock :: Parser [[Constraint']]
parseDefineBlock = parseBlock "define" $ many $ try parseConstraintsBlock <|> ([] <$ skipBlock)

parseConstraints :: Parser Constraints
parseConstraints = pSpace *> (concat <$> parseDefineBlock) <* eof

data Formula = Atom

parseLTL :: FilePath -> IO Constraints
parseLTL path = do
  content <- TextIO.readFile path
  case parseMaybe parseConstraints content of
    Just constraints -> return constraints
    Nothing -> error $ "Something went wrong while parsing " ++ path ++ "."
