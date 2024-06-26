module Main (main) where

import Control.Monad (void)
import Options.Applicative
import Solver (exampleAirport, exampleRover, solvePDDL, solveSAS)
import Text.Read (readMaybe)
import Types (Encoding (..), Options (..))

parseSCP :: Parser Double
parseSCP = option auto (long name <> metavar "PROBABILITY" <> value 0 <> help description)
  where
    name = "soft-constraint"
    description = "The probability that each soft constraint is converted to a hard constraint."

parseConvertToLTL :: Parser Bool
parseConvertToLTL = switch (long "to-LTL" <> help description)
  where
    description = "Convert the pddl constraints to LTL before handing them to the SAT-solver."

parseMaxSteps :: Parser Int
parseMaxSteps = option auto (long name <> short 'M' <> metavar "INT" <> value 50 <> help description)
  where
    name = "max-time-steps"
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
    modifier = long "encoding" <> short 'e' <> metavar "ENCODING" <> value ExistsStep <> help description
    description =
      "The encoding used for the SAT-solver, this is either \"sequential\" for a sequential encoding "
        ++ "or \"exists-step\" for a parallel encoding using exists step."

parsePDDLOptions :: Parser Options
parsePDDLOptions = Options <$> parseSCP <*> parseConvertToLTL <*> parseMaxSteps <*> parseEncoding

parseSASOptions :: Parser Options
parseSASOptions = Options 0 False <$> parseMaxSteps <*> parseEncoding

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
    describe = fullDesc <> progDesc "TODO" <> header "Welcome to LTLplanSAT"

main :: IO ()
main = do
  cmd <- execParser parser
  case cmd of
    PDDL domain problem options -> solvePDDL options domain problem
    SAS file options -> void $ solveSAS options file
    Rovers n options -> exampleRover options n
    Airport n options -> exampleAirport options n