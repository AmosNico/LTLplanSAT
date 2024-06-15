{-# LANGUAGE OverloadedStrings #-}

module ParsePDDLConstraint (parsePDDLConstraints, Constraint (..), Constraints(..)) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, noneOf, parseMaybe, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1, string)
import Text.Megaparsec.Char.Lexer (decimal, skipLineComment, space, symbol)
import Text.Megaparsec.Stream (Token)

type Predicate = ByteString

data Constraint
  = AtEnd Predicate
  | Always Predicate
  | Sometime Predicate
  | Within Int Predicate
  | AtMostOnce Predicate
  | SometimeAfter Predicate Predicate
  | SometimeBefore Predicate Predicate
  | AlwaysWithin Int Predicate Predicate
  | HoldDuring Int Int Predicate
  | HoldAfter Int Predicate
  deriving (Show)

data Constraints = Constraints {hardConstraints :: [Constraint], softConstraints :: [Constraint]}

instance Semigroup Constraints where
  Constraints hc1 sc1 <> Constraints hc2 sc2 =
    Constraints (hc1 <> hc2) (sc1 <> sc2)

instance Monoid Constraints where
  mempty = Constraints mempty mempty

singleHard, singleSoft :: Constraint -> Constraints
singleHard c = Constraints [c] []
singleSoft c = Constraints [] [c]


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

parseName :: Parser [Token Input]
parseName = some (alphaNumChar <|> char '_')

parsePredicate :: Parser Predicate
parsePredicate = parens $ do
  name <- parseName
  space1
  args <- some (parseName <* pSpace)
  return $ C8.pack $ name ++ "(" ++ intercalate ", " args ++ ")"

pKey :: Input -> Parser Input
pKey keyword = string keyword <* pSpace

parseConstraint' :: Parser Constraint
parseConstraint' =
  parens
    ( choice
        [ AtEnd <$> (pKey "at end" *> parsePredicate),
          Within <$> (pKey "within" *> decimal) <*> parsePredicate,
          AtMostOnce <$> (pKey "at-most-once" *> parsePredicate),
          SometimeAfter <$> (pKey "sometime-after" *> parsePredicate) <*> parsePredicate <?> "sometime-after",
          SometimeBefore <$> (pKey "sometime-before" *> parsePredicate) <*> parsePredicate <?> "sometime-before",
          Sometime <$> (pKey "sometime" *> parsePredicate),
          AlwaysWithin <$> (pKey "always-within" *> decimal) <*> parsePredicate <*> parsePredicate,
          Always <$> (pKey "always" *> parsePredicate),
          HoldDuring <$> (pKey "hold-during" *> decimal) <*> decimal <*> parsePredicate,
          HoldAfter <$> (pKey "hold-after" *> decimal) <*> parsePredicate
        ]
        <?> "constraint"
    )

parseSoftConstraint :: Parser Constraints
parseSoftConstraint = singleSoft <$> (pKey "preference" *> some alphaNumChar *> pSpace *> parseConstraint')

parseHardConstraint :: Parser Constraints
parseHardConstraint = singleHard <$> parseConstraint'

parseConstraint :: Parser Constraints
parseConstraint = parens (parseSoftConstraint <|> parseHardConstraint)

skipBlock :: Parser ()
skipBlock = void $ parens $ many (void (someBut "()") <|> skipBlock <?> "skipping this part")

parseBlock :: Input -> Parser a -> Parser a
parseBlock keyword pInner = parens (string keyword *> pSpace *> pInner <?> show keyword)

parseAndBlock :: Parser Constraints
parseAndBlock = parseBlock "and" (mconcat <$> many parseConstraint)

parseConstraintsBlock :: Parser Constraints
parseConstraintsBlock = parseBlock ":constraints" $ try parseAndBlock <|> parseConstraint

parseDefineBlock :: Parser [Constraints]
parseDefineBlock = parseBlock "define" $ many $ try parseConstraintsBlock <|> (mempty <$ skipBlock)

parseConstraints :: Parser Constraints
parseConstraints = pSpace *> (mconcat <$> parseDefineBlock) <* eof

parsePDDLConstraints :: FilePath -> IO Constraints
parsePDDLConstraints path = do
  content <- TextIO.readFile path
  case parseMaybe parseConstraints content of
    Just constraints -> return constraints
    Nothing -> error $ "Something went wrong while parsing the constraints in " ++ path ++ "."
