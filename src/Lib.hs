module Lib (entryp) where
  
import qualified AOC2021.Day4
import Options.Applicative

data Options = Options
  { day :: Day,
    inputType :: InputType
  }

data Day = Day {x :: Int} deriving Show

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


validateDay::String -> Either String Int
validateDay d
 | n > 0 && n < 26 = Right (n)
 | otherwise = Left ("Invalid input for day " ++ d)
 where n = read d -- Probably I shouldn't use read


inputDay :: Parser Day
inputDay = Day <$> argument (eitherReader validateDay) (metavar "DAY" <> help "Exercise day to run")


options :: Parser Options
options = Options <$> inputDay <*> inputTypeParser

cli :: IO ()
cli = run =<< execParser opts
  where
    opts = info (options <**> helper)
     (fullDesc
     <> progDesc "Run AOC exercise for a certain day"
     <> header "AOC in haskell"
     )

run :: Options -> IO ()
run (Options d (InputFile f)) = putStr $ "Running day" ++ (show d) ++ " with input file: " ++ f
run (Options d (InputString s)) = putStr $ "Running day" ++ (show d) ++ " with input string: " ++ s

entryp:: IO ()
entryp = AOC2021.Day4.run "resources/2021/day4_sample.txt"