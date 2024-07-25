module Main (main) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import Data.Set (fromList)
import Options.Applicative
import Solver (Encoding (..), Options (..), SelectSoftConstraints (..), exampleAirport, exampleRover, solvePDDL, solveSAS)
import Text.Read (readMaybe)

parseSSCRandom :: Parser SelectSoftConstraints
parseSSCRandom = SelectRandom <$> option auto modifier
  where
    modifier =
      long "select-random-soft-constraint"
        <> short 'r'
        <> metavar "PROBABILITY"
        <> value 0
        <> showDefault
        <> help description
    description = "Randomly convert soft constraints to hard constraints with the given probability."

parseSSCGiven :: Parser SelectSoftConstraints
parseSSCGiven = SelectGiven <$> option reader modifier
  where
    reader = maybeReader $ Just . fromList . C8.split ',' . C8.pack
    modifier =
      long "select-given-soft-constraint"
        <> short 'g'
        <> metavar "LIST"
        <> help description
    description =
      "Convert the given soft constraints (a list of identifiers of constraints as given in "
        ++ "the pddl problem file, separated by commas) to hard constraints."

parseSSC :: Parser SelectSoftConstraints
parseSSC = parseSSCRandom <|> parseSSCGiven

parseConvertToLTL :: Parser Bool
parseConvertToLTL = switch (long "to-LTL" <> help description)
  where
    description = "Convert the pddl constraints to LTL before handing them to the SAT-solver."

parsePrintPT :: Parser Bool
parsePrintPT = switch (short 'p' <> long "print-planning-task" <> help description)
  where
    description = "Show the the planning task that is given to the solver."

parseMaxSteps :: Parser Int
parseMaxSteps = option auto modifier
  where
    modifier =
      long "max-time-steps"
        <> short 'M'
        <> metavar "INT"
        <> value 50
        <> showDefault
        <> help description
    description =
      "The maximum number of timesteps the SAT-solver should try before judging "
        ++ "the problem to be unsolvable. Note that for the ExistsStep encoding this is not equal to "
        ++ "the number of actions."

readMaybeEncoding :: String -> Maybe Encoding
readMaybeEncoding "sequential" = Just Sequential
readMaybeEncoding "exists-step" = Just ExistsStep
readMaybeEncoding _ = Nothing

parseEncoding :: Parser Encoding
parseEncoding = option (maybeReader readMaybeEncoding) modifier
  where
    modifier =
      long "encoding"
        <> short 'e'
        <> metavar "ENCODING"
        <> value ExistsStep
        <> showDefault
        <> help description
    description =
      "The encoding used for the SAT-solver, this is either \"sequential\" for a sequential encoding "
        ++ "or \"exists-step\" for a parallel encoding using exists step."

parseValidate :: Parser Bool
parseValidate = switch (long "VAL" <> help description)
  where
    description = "Validate the found solution with VAL."

parsePDDLOptions :: Parser Options
parsePDDLOptions =
  Options
    <$> parseSSC
    <*> parseConvertToLTL
    <*> parsePrintPT
    <*> parseMaxSteps
    <*> parseEncoding
    <*> parseValidate

parseSASOptions :: Parser Options
parseSASOptions =
  Options (SelectRandom 0) False
    <$> parsePrintPT
    <*> parseMaxSteps
    <*> parseEncoding
    <*> pure False

data Command
  = PDDL FilePath FilePath Options
  | SAS FilePath Options
  | Rovers Int Options
  | Airport Int Options

commandPDDL :: Mod CommandFields Command
commandPDDL = command "pddl" (info parsePDDL (progDesc description))
  where
    description = "Run the solver on the given domain and problem files."
    parsePDDL =
      PDDL
        <$> strArgument (metavar "DOMAIN")
        <*> strArgument (metavar "PROBLEM")
        <*> parsePDDLOptions

commandSAS :: Mod CommandFields Command
commandSAS = command "sas" (info parseSAS (progDesc description))
  where
    description = "Run the solver on the given SAS-file."
    parseSAS =
      SAS
        <$> strArgument (metavar "FILEPATH")
        <*> parseSASOptions

readInRange :: Int -> Int -> String -> Maybe Int
readInRange l u s = do
  n <- readMaybe s
  if l <= n && n <= u
    then Just n
    else Nothing

commandRovers :: Mod CommandFields Command
commandRovers = command "rovers" (info parseRovers (progDesc description)) <> metavar "rovers"
  where
    description = "Run the solver on the rovers domain with specified problem file (as an Int between 1 and 20)."
    parseRovers =
      Rovers
        <$> argument (maybeReader $ readInRange 1 20) (metavar "INT")
        <*> parsePDDLOptions

commandAirport :: Mod CommandFields Command
commandAirport = command "airport" (info parseAirport (progDesc description)) <> metavar "airport"
  where
    description = "Run the solver on the specified airport SAS-file. These problems are mostly for debugging purposes."
    parseAirport =
      Airport
        <$> argument (maybeReader $ readInRange 1 4) (metavar "INT")
        <*> parseSASOptions

parseCommand :: Parser Command
parseCommand = hsubparser $ commandPDDL <> commandSAS <> commandAirport <> commandRovers

parser :: ParserInfo Command
parser = info (parseCommand <**> helper) describe
  where
    describe = fullDesc <> progDesc description <> header "Welcome to LTLplanSAT"
    description = "LTLplanSAT is a SAT based planner focussed on working with constraints. " 
     ++ "The source code can be found on https://github.com/AmosNico/LTLplanSAT."

main :: IO ()
main = do
  cmd <- execParser parser
  case cmd of
    PDDL domain problem options -> solvePDDL options domain problem
    SAS file options -> void $ solveSAS options file
    Rovers n options -> exampleRover options n
    Airport n options -> exampleAirport options n