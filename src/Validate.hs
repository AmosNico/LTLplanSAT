{-# LANGUAGE OverloadedStrings #-}

module Validate (validatePlan) where

import Basic (actionName, showNamedList)
import Constraints (Constraints, activeConstraints, constraintID)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import SequentialSAT (Plan (..))
import System.Process (readProcess)
import Text.Megaparsec (Parsec, anySingle, eof, errorBundlePretty, many, runParser, skipManyTill, some, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, space1, string)
import Text.Megaparsec.Char.Lexer (decimal, float)

writePlan :: Plan -> IO ()
writePlan (Plan as) =
  C8.writeFile "plan.txt" $
    C8.concat $
      map (\a -> C8.concat ["(", actionName a, ")\n"]) as

data Result = InvalidPlan String | ConstraintsViolated (Set ByteString) | ValidPlan

instance Show Result where
  show (InvalidPlan s) = "The plan is not valid:" ++ s
  show (ConstraintsViolated cs) =
    showNamedList
      "The plan is valid, but the following constraints are violated:"
      (Set.toList cs)
  show ValidPlan = "The plan is valid and all selected constraints are satisfied."

type Parser = Parsec Void String

parsePlanFailed :: Parser Result
parsePlanFailed = string "Plan failed to execute" *> (InvalidPlan <$> many anySingle) <* eof

parseConstraint :: Parser ByteString
parseConstraint = (C8.pack <$> some alphaNumChar) <* string ": " <* (decimal :: Parser Int) <* space1

parseConstraints :: Constraints a -> Parser Result
parseConstraints constraints = do
  violated <- Set.fromList <$> many (try parseConstraint)
  let relevant = Set.fromList $ map constraintID $ activeConstraints constraints
  let relevantViolated = Set.intersection violated relevant
  if null relevantViolated
    then return ValidPlan
    else return $ ConstraintsViolated relevantViolated

parsePlanValid :: Constraints a -> Parser Result
parsePlanValid constraints =
  string "Plan valid"
    *> space1
    *> string "Final value: "
    *> (float :: Parser Float)
    *> space1
    *> string "Violations:"
    *> space1
    *> parseConstraints constraints
    <* string "Successful plans:"

parseResult :: Constraints a -> Parser Result
parseResult constraints = skipManyTill anySingle (parsePlanFailed <|> parsePlanValid constraints)

interpretOutput :: Constraints a -> String -> String
interpretOutput constraints output = case runParser (parseResult constraints) "" output of
  Left bundle -> "The parser could not parse the output of VAL:\n" ++ errorBundlePretty bundle
  Right result -> show result

validatePlan :: FilePath -> FilePath -> Constraints a -> Plan -> IO ()
validatePlan domain problem constraints plan = do
  putStrLn "Checking the plan using Val."
  writePlan plan
  output <- readProcess "Val/bin/Validate" ["-v", domain, problem, "plan.txt"] ""
  putStrLn $ interpretOutput constraints output