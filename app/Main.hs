module Main (main) where

import Lib

-- https://www.stephanschiffels.de/posts/2021-03-24-Haskell-CLI/
-- https://www.reddit.com/r/haskell/comments/capuz7/multiple_executable_in_project/
-- https://github.com/pcapriotti/optparse-applicative
-- stack ghci projects-playground-hs:exe:cli
-- https://www.fpcomplete.com/haskell/library/optparse-applicative/


main :: IO ()
main = entryp
