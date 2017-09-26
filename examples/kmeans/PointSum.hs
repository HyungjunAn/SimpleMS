{-# LANGUAGE DeriveGeneric #-}

module PointSum where

import Data.Binary
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Vector

data PointSum = PointSum Int Double Double deriving (Generic)

instance Binary PointSum
