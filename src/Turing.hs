{-# LANGUAGE DeriveAnyClass, TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction #-}

module Turing (
    beaver
  , printSnapShots
               ) where


import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class

import Tape
import Machine
import Program
import CleanShow



evalInstruction = program toFunction <*> use machineState <*> use (tape.underHead)

type MachineState q a x = StateT (Machine q a) (ReaderT (Program q a) IO) x

singleStep :: (Ord a, Ord q) => MachineState q a ()
singleStep = do
  (q, a, dir) <- evalInstruction
  tape.underHead .= a
  machineState .= q
  tape %= move dir

snapShots a@(action, lastaction) = do
  q <- use machineState
  case q of
    Accept -> lastaction >> pure ()
    Reject -> lastaction >> pure ()
    _ -> action >> singleStep >> snapShots a


printTape f = do
  i <- cleanShow <$> ((,) <$> use machineState <*> use (tape.underHead))
  o <- cleanShow <$> evalInstruction
  t <- use (tape.(showTapeContents f) )
  q <- cleanShow <$> use machineState
  --liftIO $ putStrLn (t ++ "            " ++ q ++ ":  " ++ i ++ " -> " ++ o)
  liftIO $ putStrLn ("`Tape: " ++ t ++ "...`    $\\delta$" ++ i ++ " = " ++ o ++ "    ")
  --liftIO $ putStrLn ""


printHaltedTape f = do
  t <- use (tape.(showTapeContents f) )
  q <- cleanShow <$> use machineState
  --liftIO $ putStrLn (t ++ "            " ++ q)
  liftIO $ putStrLn ("`Tape: " ++ t ++ "...`    " ++ q ++ "    ")


runSnapShots :: (Ord a, Ord q, Show a) => (MachineState q a (), MachineState q a ())  ->  Program q a -> Tape a -> IO ()
runSnapShots action program tape = runReaderT (evalStateT (snapShots action) (machine iq tape)) program
  where iq = startState program


printSnapShots f = runSnapShots (printTape f, printHaltedTape f)


data Beaver = A | B | C deriving (Eq, Ord, Show, CleanShow)

myf = fromFunction (St A) f
  where f (St A) Blank = (St B, Letter (1 :: Int), R)
        f (St B) Blank = (St A, Letter 1, L)
        f (St C) Blank = (St B, Letter 1, L)

        f (St A) (Letter 1) = (St C, Letter 1, L)
        f (St B) (Letter 1) = (St B, Letter 1, R)
        f (St C) (Letter 1) = (Accept, Letter 1, R)

showSt (St x) = x
showSt y = show y

showLetter (Letter x) = x
showLetter _ = "_"

add1 = fromFunction (St A) f
  where f (St A) Blank = (Accept, Letter 1, NC)
        f (St A) (Letter 1) = (St A, Letter 1, R)


beaver = printSnapShots show  myf blankTape -- add1 (input [Just 1, Just 1, Just 1, Just 1, Just 1, Just 1])
--beaver = print "ok" -- runReaderT (evalStateT (replicateM_ 5 $ singleStep >> printTape) (machine (St A) blankTape)) myf
