{-# LANGUAGE OverloadedStrings #-}

module ParsePDDLConstraints (parsePDDLConstraints) where

import PDDLConstraints (PDDLConstraint (..), PDDLConstraints (..), singleHard, singleSoft)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, noneOf, parseMaybe, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1, string)
import Text.Megaparsec.Char.Lexer (decimal, skipLineComment, space, symbol)
import Text.Megaparsec.Stream (Token)
import Basic (Fact (PosAtom), Atom (Atom))

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

parsePredicate :: Parser Fact
parsePredicate = parens $ do
  name <- parseName
  space1
  args <- some (parseName <* pSpace)
  return $ PosAtom $ Atom $ C8.pack $ name ++ "(" ++ intercalate ", " args ++ ")"

pKey :: Input -> Parser Input
pKey keyword = string keyword <* pSpace

parsePDDLConstraint :: Parser PDDLConstraint
parsePDDLConstraint =
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

parseSoftConstraint :: Parser PDDLConstraints
parseSoftConstraint = singleSoft <$> (pKey "preference" *> some alphaNumChar *> pSpace *> parsePDDLConstraint)

parseHardConstraint :: Parser PDDLConstraints
parseHardConstraint = singleHard <$> parsePDDLConstraint

parseConstraint :: Parser PDDLConstraints
parseConstraint = parens (parseSoftConstraint <|> parseHardConstraint)

skipBlock :: Parser ()
skipBlock = void $ parens $ many (void (someBut "()") <|> skipBlock <?> "skipping this part")

parseBlock :: Input -> Parser a -> Parser a
parseBlock keyword pInner = parens (string keyword *> pSpace *> pInner <?> show keyword)

parseAndBlock :: Parser PDDLConstraints
parseAndBlock = parseBlock "and" (mconcat <$> many parseConstraint)

parseConstraintsBlock :: Parser PDDLConstraints
parseConstraintsBlock = parseBlock ":constraints" $ try parseAndBlock <|> parseConstraint

parseDefineBlock :: Parser [PDDLConstraints]
parseDefineBlock = parseBlock "define" $ many $ try parseConstraintsBlock <|> (mempty <$ skipBlock)

parseConstraints :: Parser PDDLConstraints
parseConstraints = pSpace *> (mconcat <$> parseDefineBlock) <* eof

parsePDDLConstraints :: FilePath -> IO PDDLConstraints
parsePDDLConstraints path = do
  content <- TextIO.readFile path
  case parseMaybe parseConstraints content of
    Just constraints -> return constraints
    Nothing -> error $ "Something went wrong while parsing the constraints in " ++ path ++ "."
