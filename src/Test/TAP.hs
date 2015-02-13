{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

{-|

Module      : Test.TAP
Description : Produce TAP output from Haskell tests
Copyright   : (c) Martyn J. Pearce 2014,2015
License     : BSD
Maintainer  : haskell@sixears.com

Produce TAP-format output for Haskell tests, and some simple test facilities to
make simple testing easy (e.g., 'is', 'ok', etc.); and provide easy-to-read
diagnostics (e.g., 'like').  Special attention is given to the diagnostic
output, to make it easy to see differences; lists are written out in an
annotated table ('like'), other things are 'show'n and the diffs highlighted
with terminal highlighting/underlining, etc.

 -}

module Test.TAP
  ( -- * Synopsis
    
    {- | @
         test [ -- simple test, always succeeds
                ok   True  "-ok test-"
                -- always fails, includes some diagnostic when it does
              , okay False "-not ok test-" ["some more diag"]
                -- this is caught, causing a test fail but continues
              , okay undefined "-explode-" ["bang"]
              , diag "this is a diagnostic string (no test)"
              , diag [ cpack "this is a "
                     , cpack "red" <> fore red <> back white
                     , cpack " word"
                     ]
                -- compare two things, output the diffs using terminal 
                -- highlighting
              , is   "foo" "bar"  "-is test-"
                -- compare two lists, output diffs in an explanatory table
              , like ([1,2,3,6] :: [Int]) [5,1,4,3] "-not like-"
                -- integrated quickcheck property test
              , check prop_wrap "prop_wrap"
              ]
         @
     -}
    Test( Diag ), 
    check, diag, explain, is, like, ok, okay, test, test_ 
  )
where

-- base --------------------------------

import Control.Exception       ( SomeException, evaluate, try )
import Control.Monad.IO.Class  ( MonadIO( liftIO ) )
import Control.Monad.State     ( execStateT, get, put )
import Data.List               ( intercalate, unzip4 )
import Data.Maybe              ( fromJust, isJust )
import Data.Monoid             ( (<>) )
import System.Exit             ( ExitCode( ExitFailure )
                               , exitSuccess, exitWith
                               )
import System.IO               ( hPutStrLn, stderr )
import System.IO.Unsafe        ( unsafePerformIO )
import Text.Printf             ( printf )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- Diff --------------------------------

import qualified Data.Algorithm.Diff as DAD

-- QuickCheck --------------------------

import Test.QuickCheck  ( Args( chatty ), Testable
                        , output, quickCheckWithResult, stdArgs )
import Test.QuickCheck.Test  ( isSuccess )

-- rainbow -----------------------------

import Rainbow  ( Chunk(..)
                , fromText, bold, inverse, underline, flash )

-- text --------------------------------

import qualified Data.Text as T

-- local-packages ------------------------------------------

-- rainbow-lines -----------------------

import Console.Rainbow  ( CLines, clines )

-- text-block --------------------------

import Text.Block  ( HAlign( HRight ), TableOptions(..)
                   , VAlign( VTop )
                   , hjoin , mkBlock, mkBlockn
                   , table'
                   )

-- this package --------------------------------------------

import Test.TAP.Diagable  ( Diagable( diagable ) )
import Test.TAP.Types     ( Test(..), TestStateIO, isTest )

--------------------------------------------------------------------------------

-- diff ------------------------------------------------------------------------

-- | description of differences between two lists (note: positional; this is
--   the difference of two lists, not two sets)

data Diff a = First a     -- ^ item appears in the first list only
            | Second a    -- ^ item appears in the second list only
            | Both a a    -- ^ item appears in both lists
            | Change a a  -- ^ item in first list is changed for item in second
                          --   list
  deriving (Eq, Show)

-- | generate a list of differences between xs & ys

diff :: Eq a => [a] -> [a] -> [Diff a]
diff xs ys =
  collapse Nothing $ DAD.getDiff xs ys
  where
    -- take a list of DAD.Diffs, convert to Diffs, spotting patterns of
    -- n -x- First followed by n -x- Second; collapsing those into n -x- Change

    collapse :: Eq a => Maybe (Either [a] [a]) -> [DAD.Diff a] -> [Diff a]

    collapse_ (Just (Left  firsts))   =  fmap First  firsts
    collapse_ (Just (Right seconds))  =  fmap Second seconds
    collapse_ Nothing                 =  []

    collapse__  (DAD.First  a : ds)  = collapse (Just $ Left [a]) ds
    collapse__  (DAD.Second a : ds)  = collapse (Just $ Right [a]) ds
    collapse__  (DAD.Both a b : ds)  = Both a b : collapse Nothing ds
    collapse__  []                   = error "collapse__: empty list"

    collapse pending [] = collapse_ pending

    collapse Nothing            ds  =  collapse__ ds
    collapse (Just (Left []))   ds  =  collapse__ ds
    collapse (Just (Right []))  ds  =  collapse__ ds

    collapse (Just (Left   firsts))  (DAD.First a : ds)  =
      collapse (Just $ Left (firsts ++ [a])) ds
    collapse (Just (Right  seconds)) (DAD.Second a : ds)  =
      collapse (Just $ Right (seconds ++ [a])) ds
    collapse pending (DAD.Both a b : ds) =
      collapse_ pending ++ [Both a b] ++ collapse Nothing ds

    collapse (Just (Left (f:fs)))  (DAD.Second a : ds)  =
      Change f a : collapse (Just $ Left fs) ds
    collapse (Just (Right (s:ss)))  (DAD.First a : ds)  =
      Change a s : collapse (Just $ Right ss) ds

-- diffMarkers -----------------------------------------------------------------

diffMarkers :: String -> String -> (String,String)
diffMarkers xs ys  =  let dx (Both _ _)   = " "
                          dx (First _)    = "+"
                          dx (Second _)   = ""
                          dx (Change _ _) = "^"
                          dy (Both _ _)   = " "
                          dy (First _)    = ""
                          dy (Second _)   = "+"
                          dy (Change _ _) = "^"
                          diffs           = diff xs ys
                       in (dx =<< diffs, dy =<< diffs)

-- trim ------------------------------------------------------------------------

-- | trim a string to n places, replacing with another string if needed
--   e.g., ensure a string is no more than 80 chars, replacing end with '...'
--   if required:  trim 80 "..." s

trim :: Int -> String -> String -> String
trim n r s = if length s > n
             then take (n - length r) s ++ r
             else s

-- exit ------------------------------------------------------------------------

-- | akin to C's _exit(int); exit process with a given value

exit :: Int -> IO a
exit 0 = exitSuccess
exit e = _exit e

_exit :: Int -> IO a
_exit = exitWith . ExitFailure

-- ePutStrLns --------------------------

-- | write lines (each with an added newline) to stderr

ePutStrLns :: [String] -> IO()
ePutStrLns = mapM_ (hPutStrLn stderr)

-- test_ok -----------------------------

test_ok :: String -> x -> TestStateIO ()
test_ok n _ = do
  (i, fails) <- get
  liftIO . putStrLn $ printf "ok %d - %s" (i+1) n
  put (i+1, fails)
  return ()

test_fail :: String -> CLines -> TestStateIO ()
test_fail n cls = do
  (i, fails) <- get
  liftIO . putStrLn $ printf "not ok %d - %s" (i+1) n
  test_failed cls i fails
--  liftIO $ diagable cls
--  put (i+1, i+1 : fails)
--  return ()

test_failed :: CLines -> Int -> [Int] -> TestStateIO()
test_failed cls i fails = do
  liftIO $ diagable cls
  put (i+1, i+1 : fails)
  return ()

test_except :: String -> SomeException -> CLines -> TestStateIO ()
test_except n e cls = do
  (i, fails) <- get
  liftIO . putStrLn $ printf "not ok %d - %s" (i+1) n
  liftIO . diag $ "!! " ++ show e
  test_failed cls i fails
--  liftIO $ diagable cls
--  put (i+1, i+1 : fails)
--  return ()

-- | map a test to a TestStateIO transformer according to whether it is
--   successful or not
test_state :: Test -> TestStateIO ()
test_state (Diag ds)     = liftIO $ diagable ds
test_state (Test b n ds) = do
  t <- liftIO $ try (evaluate b)
  case t of
    Right True                 ->  test_ok   n ds
    Right False                ->  test_fail n ds
    Left e                     ->  test_except n e ds


--------------------------------------------------------------------------------

cpack :: String -> Chunk
cpack = fromText . T.pack

-- is --------------------------------------------------------------------------

{- | test if two values are equal (per '(==)').  If not, stringify the results
     and display the differences.  Where one character is changed for another,
     the text is inverted; where one character is inserted (on either side), the
     text is underlined, bolded, and blinking (where the terminal supports it).
 -}

is :: (Eq a, Show a) => a
                     -> a
                     -> String
                     -> Test
is x y s = let diffs = diff (show x) (show y)
               changed c = [cpack [c] <> inverse]
               extra   c = [cpack [c] <> underline <> bold <> flash]
               normal  c = [cpack [c]]
               got  = (\d -> case d of
                                Change c _ -> changed c
                                First  c   -> extra c
                                Second _   -> []
                                Both   c _ -> normal c
                             ) =<< diffs
               expt = (\d -> case d of
                               Change _ c -> changed c
                               First  _   -> []
                               Second c   -> extra c
                               Both   _ c -> normal c
                            ) =<< diffs
            in okay (x == y) s [ cpack "     got: " : got
                               , cpack "expected: " : expt
                               ]

-- check -----------------------------------------------------------------------

-- | perform a QuickCheck check within a test context (thus producing TAP
--   output)

check :: Testable prop => prop -> String -> Test
check prop s = unsafePerformIO $ do
  r <- quickCheckWithResult stdArgs { chatty = False } prop
  return $ okay (isSuccess r) s (output r)

-- like ------------------------------------------------------------------------

-- | maybe a pair of columns showing the diff from a to b and b to a,
--   if applicable
_diff_l      :: (Show t, Eq t) => Maybe t -> Maybe t -> Maybe (String, String)
_diff_l a b  =  let sj = show . fromJust
                 in if isJust a && isJust b && a /= b
                    then Just $ diffMarkers (sj a) (sj b)
                    else Nothing

-- | given two maybe vals, give '*' if they differ, '<' if left exists only,
--   '>' if right exists only, ' ' if all the same
_marker :: Eq a => Maybe a -> Maybe a -> Char
_marker (Just a) (Just b) | a == b    = ' '
                          | otherwise = '*'
_marker (Just _) Nothing              = '<'
_marker Nothing  (Just _)             = '>'
_marker Nothing  Nothing              = error "_marker Nothing Nothing"

-- | build a single display row up (before slicing into a possible 'diff' row)
--   from a row num and two values

_build_row :: (Eq a, Show a) =>
              Int -> Maybe a -> Maybe a ->
              (Int, Char, String, String, Maybe (String, String))
_build_row i x y =
  (i, _marker x y, maybe "" show x, maybe "" show y, _diff_l x y)

-- | compare two lists, if they do not equal, provide diagnostics as a big table
--   to show the rows that differ

like :: (Eq a, Show a) => [a] -> [a] -> String -> Test
like xs ys s = let xs_ys = fmap unwrap_diff $ diff xs ys
                           where unwrap_diff (First  a)    =  (Just a, Nothing)
                                 unwrap_diff (Second a)    =  (Nothing, Just a)
                                 unwrap_diff (Both a b)    =  (Just a, Just b)
                                 unwrap_diff (Change a b)  =  (Just a, Just b)
                   -- raw rows (before diving into main row &
                   -- possible 'diff' row)
                   -- rows     :: [(Int, Char, Maybe a, Maybe a, Maybe (a, a))]
                   rows     = uncurry (zipWith3 _build_row [0..]) (unzip xs_ys)
                   unfold u =
                       unfold1 u : unfold2 u
                     where unfold1 (i,c,a,b,_) = (Just i, c, a, b)
                           unfold2 (_,_,_,_,d) =
                             case d of
                               Just (d1,d2) -> [( Nothing , ' ' , d1, d2 )]
                               Nothing      -> []

                   (rnums, markers, xs_, ys_) = unzip4 $ unfold =<< rows
                   -- rownum column, as a Block
                   rcol     = mkBlockn HRight 2 $ fmap (maybe "" show) rnums
                   col0     = hjoin VTop [ rcol, mkBlock $ fmap (:[]) markers ]
                   mytable  = table' def { table_col_separator = "|" }
                   header   = fmap mkBlock [[""], ["got"], ["exp"]]
                   t_rows   = [ header, [ col0 , mkBlock xs_ , mkBlock ys_] ]
                in okay (xs == ys) s (lines . show $ mytable t_rows)

-- ok --------------------------------------------------------------------------

-- | the simplest of all tests

ok :: Bool    -- ^ test pass or fail
   -> String  -- ^ test name
   -> Test
ok b s = okay b s ([] :: [String])

-- okay ------------------------------------------------------------------------

-- | the root of all tests; check the value, format output as TAP

okay :: Chunky c => Bool    -- ^ test pass or fail
                 -> String  -- ^ test name
                 -> c       -- ^ diagnostic output, if any
                 -> Test
okay b s es = Test b s (chunky es)

-- diag ------------------------------------------------------------------------

-- | Things which may be output to a termninal - as groups of lines - thus
--   strings, but also Rainbow Chunks

class Chunky a where
  chunky :: a -> CLines

instance Chunky [[Chunk]] where
  chunky = clines

instance Chunky [Chunk] where
  -- :: [Chunk] -> CLines
  chunky cs = clines [cs]

instance Chunky String where
  -- :: String -> CLines
  chunky s = chunky [s]

instance Chunky [String] where
  -- :: [String] -> CLines
  chunky = clines . fmap (return . cpack)

-- | write a string to stdout with a leading # on each line

diag :: (Diagable d, Chunky s) => s -> d
diag s = diagable (chunky s)

-- explain ---------------------------------------------------------------------

-- | write a 'Show'able to stdout, like diag, with a preceding name

explain :: (Show s, Diagable d) => String  -- ^ name to use
                                -> s       -- ^ thing to show
                                -> d
explain s x = diag $ s ++ ": " ++ show x

-- test ------------------------------------------------------------------------

-- | perform a set of tests, writing TAP output to stdout, return the error code
--   where 0x2 is wrong count of tests run; 0x4 is one or more tests failed

test_ :: [Test] -> IO Int
test_ ts = do
  let test_count  =  length $ filter isTest ts
      -- check for ok condition; if true, return 0; else print some error,
      -- and return exit bits
      e_check b r es =
        if b then return 0x0 else ePutStrLns (fmap ("## " ++) es) >> return r
  case ts of
        [] -> error "no tests defined!" >> exit 0x1
        _  -> putStrLn $ printf "1..%d" test_count
  (tests_run, failed) <- (execStateT $ mapM_ test_state ts) (0,[])
  let f_prefix    = "  failed: "
      failed_s    = intercalate "," . fmap show $ reverse failed
      f_tests     = f_prefix ++ trim (80 - length f_prefix) "..." failed_s
      tests_s     = "test" ++ if 1 == length failed then "" else "s"

  e0 <- e_check (tests_run == test_count) 0x2
                [ printf "looks like you planned %d %s but ran %d"
                         test_count tests_s tests_run ]
  e1 <- e_check (null failed)             0x4
                [ printf "looks like you failed %d %s out of %d run"
                         (length failed) tests_s test_count
                , f_tests
                ]
  return $ e0 + e1

-- | like 'test_', but exit on completion (exit 0 on success, non-zero if
--   any tests fail

test :: [Test] -> IO()
test ts = test_ ts >>= exit

--------------------------------------------------------------------------------

-- That's All, Folks!