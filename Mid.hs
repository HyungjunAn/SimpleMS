{-# LANGUAGE TemplateHaskell #-}
module Mid where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)
import Control.Distributed.Process.Serializable

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Lib


--good :: ProcessId -> Process ()
let funcName = plus in good
