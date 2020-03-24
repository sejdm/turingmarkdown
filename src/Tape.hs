{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, DeriveAnyClass, NoMonomorphismRestriction #-}

module Tape (
    Tape
  , underHead
  , tapeContents
  , showTapeContents
  , move
  , Direction (..)
  , Letter (..)
  , blankTape
  , input
            ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import CleanShow

data Letter a = Letter a | Blank deriving (Eq, Ord, Show)

data Tape a = Tape [Letter a] [Letter a]

instance (CleanShow a) => CleanShow (Letter a) where
  cleanShow (Letter x) = cleanShow x
  cleanShow _ = "_"

data Direction = L | R | NC deriving (Eq, Ord, Show, CleanShow)

input xs = Tape [] xs

blankTape = Tape [] []

move L (Tape (x:xs) ys) = Tape xs (x:ys)
move L t = t
move R (Tape xs (y:ys)) = Tape (y:xs) ys
move R (Tape xs _) = Tape (Blank : xs) []
move _ t = t


atHead (Tape _ (x:_)) = x
atHead _ = Blank

onHead (Tape xs (_:ys)) x = Tape xs (x:ys)
onHead (Tape xs _) x = Tape xs [x]

underHead :: Lens' (Tape a) (Letter a)
underHead = lens atHead onHead

tapeContents :: Getter (Tape a) [Letter a]
tapeContents = to (\(Tape xs ys) -> reverse xs ++ ys)

showTapeContents :: Show a => (a -> String) -> Getter (Tape a) String
showTapeContents f = to (\(Tape xs ys) -> concat (map shw (reverse xs) ++ withHead ys))
  where shw (Letter x) = " " ++ f x ++ " "
        shw _ = " _ "
        shwHd (Letter x) = "|" ++ f x ++ "|"
        shwHd _ = "| |"
        withHead (y:ys) = shwHd y :  map shw ys
        withHead _ = ["| |"]

        showHead xs = concatMap (const "   ") xs ++ " ⬇ "
        -- pad s = "| " ++ s ++ " |"
        -- padHead s = "^ " ++ s ++ " ^"



showTapeContentsAsMarkdownTable :: Show a => (a -> String) -> Getter (Tape a) String
showTapeContentsAsMarkdownTable f = to (\(Tape xs ys) -> concat (map shw (reverse xs) ++ withHead ys))
  where shw (Letter x) = "| " ++ f x ++ " "
        shw _ = " _ "
        shwHd (Letter x) = "| **" ++ f x ++ "** "
        shwHd _ = "| **.** "
        withHead (y:ys) = shwHd y :  map shw ys
        withHead _ = ["| **.** "]

        showHead xs = concatMap (const "   ") xs ++ " ⬇ "
