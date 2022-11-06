module Lib (entryp) where
  
import qualified AOC2021.Day4
import Options.Applicative

data Options = Options
  { day :: Day,
    inputType :: InputType
  }

data Day = Day4 deriving Show

data InputType = InputFile FilePath | InputString String deriving Show

inputTypeParser :: Parser InputType
inputTypeParser = inputFileParser <|> inputStringParser
  where
    inputFileParser =
      InputFile
        <$> strOption
          ( long "file"
              <> short 'f'
              <> help "Filepath to exercise input"
          )
    inputStringParser =
      InputString
        <$> strOption
          (long "string" <> short 's' <> help "Raw input string")


validateDay::String -> Either String Day
validateDay d
 | d == "4" = Right(Day4)
 | otherwise = Left("Invalid input day: "++ d)


inputDay :: Parser Day
inputDay = argument (eitherReader validateDay) (metavar "DAY" <> help "Exercise day to run")

options :: Parser Options
options = Options <$> inputDay <*> inputTypeParser

entryp :: IO ()
entryp = run =<< execParser opts
  where
    opts = info (options <**> helper)
     (fullDesc
     <> progDesc "Run AOC exercise for a certain day"
     <> header "AOC in haskell"
     )

run :: Options -> IO ()
run (Options Day4 (InputFile f)) =  AOC2021.Day4.run f
run (Options d (InputFile f)) = putStr $ "Running year  day " ++ (show d) ++ " with input file: " ++ f
run (Options d (InputString s)) = putStr $ "Running day " ++ (show d) ++ " with input string: " ++ s