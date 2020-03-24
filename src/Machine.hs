{-# LANGUAGE TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction #-}

module Machine (
    machineState
  , tape
  , St (..)
  , Machine
  , machine
               ) where


import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Lens
import qualified Data.Map as M
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import Tape
import CleanShow


data St a = St a | Accept | Reject deriving (Eq, Ord, Show)

instance CleanShow a => CleanShow (St a) where
  cleanShow (St x) = cleanShow x
  cleanShow Accept = "Accept"
  cleanShow Reject = "Reject"

data Machine q a = Machine {
    _machineState :: St q
  , _tape :: Tape a
  }

makeLenses ''Machine

machine = Machine
