{-# LANGUAGE TemplateHaskell, TypeFamilies, TupleSections, FlexibleContexts, NoMonomorphismRestriction #-}

module Program (
    Program
  , toFunction
  , toList
  , program
  , startState
  , fromFunction
  , fromMap
  , fromList
  , textToProgram
  , parsedProgram
  , table
  , letter
  , st
  , movement
  , allowedChar
  , cleanTable
  , tableToProgram
               ) where


import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import Machine
import Tape
import Data.Void
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import CleanShow



data Program q a = Program {
    startState :: St q
  , toMap :: M.Map (St q, Letter a) (St q, Letter a, Direction)
  , toFunction :: St q -> Letter a -> (St q, Letter a, Direction)
  , toList :: [((St q, Letter a), (St q, Letter a, Direction))]
  }

fromFunction i f = Program i (M.fromList []) f []

fromMap i m = Program i m f ls
  where
    f q a = M.findWithDefault (Reject, a, NC) (q, a) m
    ls = M.toList m

fromList i = fromMap i . M.fromList

program = lift . asks


tableToPairs (header , rows) = concatMap (f header) rows
  where f h (c,cs) = zip (map (c,) h) cs

tableToProgram t@(_,rs) = fromList i $ tableToPairs t
  where i = fst $ head  rs





-- Parsing a text program represented as an Markdown table

allowedChar = satisfy (\x -> x /= ' ' && x /= '|' && x /= ',' && x /= '<' && x /= '>')
headingSepChar = satisfy (\x -> x == '|' || x == '-' || x == '+')
headingSep = try $ many (notFollowedBy newline >> headingSepChar) >> newline
bar = char '|'
possibleBar = try (bar *> pure ()) <|> pure ()
comma = char ','
spc = many (char ' ')

st = do
  x <- spc *> many (try allowedChar) <* spc
  case x of
    "accept" -> return Accept
    "acc" -> return Accept
    "reject" -> return Reject
    "re" -> return Reject
    s -> return (St s)

letter =  (f <$> (spc *> many allowedChar <* spc))
  where f "" = Blank
        f x = Letter x

movement = do
  x <- spc *> allowedChar <* spc
  case x of
    'L' -> pure L
    'R' -> pure R
    _ -> empty

output = (,,) <$> ( st <* comma) <*> ( letter <* comma) <*> movement

dbg' _ = id
row = (,) <$> ( bar *> st) <*> (some (try $ bar *> spc *> output <* spc) <* bar <* eol)

header = bar *> spc *> some (try allowedChar) *> spc *> many ( try $ bar *> notFollowedBy (spc >> newline) >>  letter) <* bar <* eol

table = ((,) <$> (header <* (headingSep <|> pure '\n'))  <*> some (try $ notFollowedBy eol >> row)) <* eol

parsedProgram = tableToProgram <$> table


textToProgram ::  String -> Either (ParseErrorBundle String Void) (Program String String)
textToProgram = runParser parsedProgram ""

-- Rendering


cleanRow (c, cs) = padding . concat . intersperse "|" $  cleanShow c : map cleanShow cs
separator h = (++"|") . ("|-"++) . concat . intersperse "|" $  "-" : map (const "-") h
cleanHead h = padding . concat . intersperse "|" $ " " : map cleanShow h
padding s = "| " ++ s ++ " |"

cleanTable (h, rs) = unlines $ cleanHead h : separator h : map cleanRow rs
