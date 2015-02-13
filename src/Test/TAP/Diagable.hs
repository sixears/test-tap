{-# LANGUAGE GADTs #-}

-- split out 'Diagable' from Test so we can set GADTs here; if we use
-- GADTs within Test.hs, we get obscure error messages such as

{-
    No instance for (Control.Monad.State.Class.MonadState [Bool] m0)
      arising from a use of `get'
    Possible fix:
      add an instance declaration for
      (Control.Monad.State.Class.MonadState [Bool] m0)
    In a stmt of a 'do' block: oks <- get
    In the expression:
      do { oks <- get;
           put (b : oks);
           return
           $ (putStrLn
              $ printf
                  "%sok %d - %s" (if b then "" else "not ") (1 + (length oks)) s)
             >> when (not b) (f es) }
    In an equation for `okay__':
        okay__ b s es
          = do { oks <- get;
                 put (b : oks);
                 return
                 $ (putStrLn
                    $ printf
                        "%sok %d - %s" (if b then "" else "not ")
                                       (1 + (length oks)) s)
                   >> when (not b) (f es) }
-}

--------------------------------------------------------------------------------

{-|

Module      : Test.TAP.Diagable
Description : typeclass Diagable, which is things that may produce diagnostics
Copyright   : (c) Martyn J. Pearce 2014,2015
License     : BSD
Maintainer  : haskell@sixears.com

 -}

module Test.TAP.Diagable
  ( Diagable( diagable ) )
where

-- base --------------------------------

import qualified System.Environment as SysEnv

import Control.Exception   ( tryJust )
import Control.Monad       ( forM_, guard )
import System.IO.Error     ( isDoesNotExistError )
import System.IO.Unsafe    ( unsafePerformIO )

-- local modules -------------------------------------------

-- rainbow-lines -----------------------

import Console.Rainbow  ( CLines, getCLines, putLine )

-- this package --------------------------------------------

import Test.TAP.Types  ( Test( Diag ) )

--------------------------------------------------------------------------------

-- getEnv ------------------------------

-- getenv returning Maybe
getEnv :: String -> Maybe String
getEnv e = unsafePerformIO $ do
           en <- tryJust (guard . isDoesNotExistError) $ SysEnv.getEnv e
           either (\ _ -> return Nothing) (return . Just) en

------------------------------------------------------------
-- Diagable
------------------------------------------------------------

-- | things which may produce diagnostics; if you can give it some CLines and
--   it will produce diagnostics in a context-appropriate fashion, then it is
--   Diagable

class Diagable t where
  diagable :: CLines -> t

instance a ~ () => Diagable (IO a) where
  -- CLines -> IO ()
  diagable cls =
    case getEnv "PROVE" of
      Just "1" -> return ()
      _        -> forM_ (getCLines cls) (\cl -> putStr "# " >> putLine cl)

instance Diagable Test where
  -- :: CLines -> Test
  diagable = Diag

