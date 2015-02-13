#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

-- base --------------------------------

import qualified System.Environment as SysEnv

import Control.Exception   ( tryJust )
import Control.Monad       ( guard )
import System.Exit         ( ExitCode( ExitFailure )
                           , exitSuccess, exitWith
                           )
import System.IO.Error     ( isDoesNotExistError )
import System.IO.Unsafe    ( unsafePerformIO )

import Data.Monoid  ( (<>) )

-- rainbow -----------------------------

import Rainbow  ( Chunk, back, fore, fromText, red, white )

-- text --------------------------------

import qualified Data.Text as T

-- this package --------------------------------------------

import Test.TAP  ( diag, explain, is, like, okay, test_ )

--------------------------------------------------------------------------------

-- exit ------------------------------------------------------------------------

-- | akin to C's _exit(int); exit process with a given value

exit :: Int -> IO a
exit 0 = exitSuccess
exit e = _exit e

_exit :: Int -> IO a
_exit = exitWith . ExitFailure

-- getenv ------------------------------

getEnv :: String -> Maybe String
getEnv e = unsafePerformIO $ do
           en <- tryJust (guard . isDoesNotExistError) $ SysEnv.getEnv e
           either (\ _ -> return Nothing) (return . Just) en

-- cpack -------------------------------

cpack :: String -> Chunk
cpack = fromText . T.pack

-- main --------------------------------

main :: IO()
main = do
  -- prove/TAP doesn't likes the plan to be the very first thing it sees; even 
  -- before any diagnostic
  case getEnv "PROVE" of
    Nothing  -> diag "before the rain"
    Just "1" -> return ()
    _        -> diag "before the rain"
  
  ex <- test_ [ okay True  "-ok test-"     ["some diag"] 
              , okay False "-not ok test-" ["some more diag"] 
              , okay undefined "-explode-" ["bang"]
              , diag ""
              , diag "this is a diagnostic string"
              , diag [ cpack "this is a "
                     , cpack "red" <> fore red <> back white
                     , cpack " word"
                     ]
              , diag ["some", "lines"]
              , is   "foo" "foo"  "-is test-"
              -- show diff strings!
              , is   "foo" "baroo"  "-is not test-"
              , like ([1,2,3] :: [Int]) [1,2,3] "-like-"
              , like ([1,2,3,6] :: [Int]) [5,1,4,3] "-not like-"
              ]
  explain "after the deluge" ([1,2,3] :: [Int])
  exit ex
