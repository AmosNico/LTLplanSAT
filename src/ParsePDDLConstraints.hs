{-# LANGUAGE OverloadedStrings #-}

module ParsePDDLConstraints (parsePDDLConstraints) where

import Basic (Atom (Atom), Fact (PosAtom))
import Constraints (Constraint (Constraint), singleHard, singleSoft)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Void (Void)
import Data.Word8 (_parenleft, _parenright, _underscore)
import PDDLConstraints (PDDLConstraints, PDDLFormula (..))
import Text.Megaparsec (Parsec, between, choice, empty, eof, many, noneOf, parseMaybe, some, try, (<?>), (<|>))
import Text.Megaparsec.Byte (alphaNumChar, char, space1, string)
import Text.Megaparsec.Byte.Lexer (decimal, skipLineComment, space, symbol)
import Text.Megaparsec.Stream (Token)

type Parser = Parsec Void ByteString

pSpace :: Parser ()
pSpace = space space1 (skipLineComment ";") empty <?> "space or comment"

pSymbol :: ByteString -> Parser ByteString
pSymbol = symbol pSpace

parens :: Parser a -> Parser a
parens p = between (pSymbol "(") (pSymbol ")") p <?> "parentheses"

someBut :: [Token ByteString] -> Parser [Token ByteString]
someBut l = some $ noneOf l

parseName :: Parser ByteString
parseName = BS.pack <$> some (alphaNumChar <|> char _underscore)

parsePredicate :: Parser Fact
parsePredicate = parens $ do
  name <- parseName
  space1
  args <- some (parseName <* pSpace)
  return $ PosAtom $ Atom $ C8.concat [name, "(", C8.intercalate ", " args, ")"]

pKey :: ByteString -> Parser ByteString
pKey keyword = string keyword <* pSpace

parsePDDLFormula :: Parser PDDLFormula
parsePDDLFormula =
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

parsePDDLConstraint :: Parser (Constraint PDDLFormula)
parsePDDLConstraint = Constraint <$> (pKey "preference" *> parseName) <*> (pSpace *> parsePDDLFormula)

parseSoftConstraint :: Parser PDDLConstraints
parseSoftConstraint = singleSoft <$> parsePDDLConstraint

parseHardConstraint :: Parser PDDLConstraints
parseHardConstraint = singleHard <$> parsePDDLConstraint

parseConstraint :: Parser PDDLConstraints
parseConstraint = parens (parseSoftConstraint <|> parseHardConstraint)

skipBlock :: Parser ()
skipBlock = void $ parens $ many (void (someBut [_parenleft, _parenright]) <|> skipBlock <?> "skipping this part")

parseBlock :: ByteString -> Parser a -> Parser a
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
  content <- C8.readFile path
  case parseMaybe parseConstraints content of
    Just constraints -> return constraints
    Nothing -> error $ "Something went wrong while parsing the constraints in " ++ path ++ "."
