module Command (entryp) where
  
import qualified AOC2021.Day4
import qualified AOC2022.Day1
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
 | (exStr == "2021-1") || (exStr == "2021-01") = Right(AOC2022.Day1.run)
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