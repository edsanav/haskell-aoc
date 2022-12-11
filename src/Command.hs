module Command (entryp) where
  
import qualified AOC2021.Day4
import qualified AOC2022.Day1
import qualified AOC2022.Day2
import qualified AOC2022.Day3
import qualified AOC2022.Day4
import qualified AOC2022.Day5
import qualified AOC2022.Day6
import qualified AOC2022.Day7
import qualified AOC2022.Day8
import qualified AOC2022.Day9
import qualified AOC2022.Day10
import qualified AOC2022.Day11

import Options.Applicative

data Options = Options
  { exercise :: Exercise,
    inputType :: InputType
  }

type Exercise = String -> IO ()

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


validateExercise::String -> Either String Exercise
validateExercise exStr
 | (exStr == "2021-4") || (exStr == "2021-04") = Right(AOC2021.Day4.run)
 | (exStr == "2022-1") || (exStr == "2022-01") = Right(AOC2022.Day1.run)
 | (exStr == "2022-2") || (exStr == "2022-02") = Right(AOC2022.Day2.run)
 | (exStr == "2022-3") || (exStr == "2022-03") = Right(AOC2022.Day3.run)
 | (exStr == "2022-4") || (exStr == "2022-04") = Right(AOC2022.Day4.run)
 | (exStr == "2022-5") || (exStr == "2022-05") = Right(AOC2022.Day5.run)
 | (exStr == "2022-6") || (exStr == "2022-06") = Right(AOC2022.Day6.run)
 | (exStr == "2022-7") || (exStr == "2022-07") = Right(AOC2022.Day7.run)
 | (exStr == "2022-8") || (exStr == "2022-08") = Right(AOC2022.Day8.run)
 | (exStr == "2022-9") || (exStr == "2022-09") = Right(AOC2022.Day9.run)
 | (exStr == "2022-10") = Right(AOC2022.Day10.run)
 | (exStr == "2022-11") = Right(AOC2022.Day11.run)
 | otherwise = Left("Invalid input exercise:" ++ exStr)


inputExercise :: Parser Exercise
inputExercise = argument (eitherReader validateExercise) (metavar "DAY" <> help "Exercise day to run in format YYYY-DD")

options :: Parser Options
options = Options <$> inputExercise <*> inputTypeParser

entryp :: IO ()
entryp = run =<< execParser opts
  where
    opts = info (options <**> helper)
     (fullDesc
     <> progDesc "Run AOC exercise for a certain day"
     <> header "AOC in haskell"
     )

run :: Options -> IO ()
run (Options ex (InputFile f)) =  readFile f >>= ex
run (Options ex (InputString s)) =  ex s