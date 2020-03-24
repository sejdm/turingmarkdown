module Labels (
  runLabel
              ) where


import Turing
import Program
import Machine
import Tape
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void
import System.Environment
import Data.Functor
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Megaparsec.Debug
import qualified Data.Map as M




spcToNothing ' ' = Blank
spcToNothing x = Letter [x]





-- Using labels

saveToLabel t s = lift $ modify (M.insert s t)


parsePutTableLabel = do
  l <- string "label: " *> many (notFollowedBy newline >> allowedChar)
  newline
  t <- table

  let ct = cleanTable t
  let p = tableToProgram t

  saveToLabel p l
  pure $ putStr (cleanTable t)


parseInputLabel = do
  l <- string "<<" *> many (notFollowedBy newline >> allowedChar) <* string ">>"
  m <- lift get
  i <- string ": " *> many (notFollowedBy newline >> allowedChar)

  pure $ case M.lookup l m of

    Just p -> putStrLn ("input: "++i)
      -- >> putStrLn "```"
      -- >> putStrLn ""
      >> putStrLn ""
      >> printSnapShots id p (input $ map spcToNothing i)
      -- >> putStrLn "```"
      >> putStrLn ""

    _ -> putStrLn ("ERROR: label \"" ++ l ++ "\" does not exist!!!")



parseBothLabel =  fmap sequence $  many (try parsePutTableLabel  <|> try parseInputLabel <|> (putChar <$> anySingle))

runLabel ::  String -> Either (ParseErrorBundle String Void) (IO [()])
runLabel s = evalState (runParserT parseBothLabel "" s) M.empty
