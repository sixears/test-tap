{-|

Module      : Test.TAP.Types
Description : Types fo Test.TAP
Copyright   : (c) Martyn J. Pearce 2014,2015
License     : BSD
Maintainer  : haskell@sixears.com

 -}

module Test.TAP.Types
  ( Test(..), TestStateIO, isTest )
where

-- base --------------------------------

import Control.Monad.State  ( StateT )

-- rainbow-lines -----------------------

import Console.Rainbow  ( CLines )

--------------------------------------------------------------------------------

-- | represent a test to run, or maybe some diagnostic text without an
--   associated test

data Test      = Test { t_ok :: Bool, t_name :: String, t_diags :: CLines }
               | Diag { diags :: CLines }
  deriving Show

-- | is this really a test, or just some diag?

isTest :: Test -> Bool
isTest (Test {}) = True
isTest _         = False

-- | current state of active testing (test index, list of failed indices)
type TestStateT a = StateT (Int, [Int]) a 

-- | TestStateT in IO monad
type TestStateIO = TestStateT IO
