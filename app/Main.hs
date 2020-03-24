{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Turing
import Program
import Machine
import Tape
import Labels
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void
import System.Environment
import Data.Functor
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Megaparsec.Debug
import qualified Data.Map as M


replaceInputs :: IO ()
replaceInputs = do
  s <- getContents

  case runLabel s of
    Right o -> o >> pure ()
    Left e -> putStr $ errorBundlePretty e

main = do
  xs <- getArgs
  case xs of
    [] -> replaceInputs
    ys:_ -> inputFromArgs ys








-- Alternative that takes input from the argument

spcToNothing ' ' = Blank
spcToNothing x = Letter [x]


tillProgram = many (try $ notFollowedBy parsedProgram >> anySingle)

textToProg ::  String -> Either (ParseErrorBundle String Void) (Program String String)
textToProg = runParser  (tillProgram *> parsedProgram) ""


inputFromArgs xs = do
  s <- getContents

  case textToProg s of

    Right p -> putStrLn ("Input: " ++ xs) >> printSnapShots id  p (input $ map spcToNothing xs)

    Left e -> putStr $ errorBundlePretty e






