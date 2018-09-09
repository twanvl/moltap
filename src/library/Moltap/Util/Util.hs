{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Util.Util
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions used throughout Moltap
--
--------------------------------------------------------------------------------

module Moltap.Util.Util where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map            (Map)
import Data.Set            (Set)
import Data.List           (intersperse)
import Data.Hashable       (hash)
import Data.Char
import Control.Exception
import Control.Monad.Trans (MonadIO(..))
import Control.Concurrent

--------------------------------------------------------------------------------
-- * Sorted lists
--------------------------------------------------------------------------------

-- | Intersection, union and differences of sorted lists
--
--  > iud a b = (intersection, union, only in a, only in b)
iud :: Ord a => [a] -> [a] -> ([a],[a],[a],[a])
iud [] ys = ([],ys,[],ys)
iud xs [] = ([],xs,xs,[])
iud xxs@(x:xs) yys@(y:ys) = case compare x y of
    LT -> let (i,u,da,db) = iud xs yys in (i,x:u,x:da,db)
    EQ -> let (i,u,da,db) = iud xs ys  in (x:i,x:u,da,db)
    GT -> let (i,u,da,db) = iud xxs ys in (i,y:u,da,y:db)

--------------------------------------------------------------------------------
-- * Relations
--------------------------------------------------------------------------------

-- | A binary relation from @a@ to @a@
type Relation a = Map a [a]

-- | Symmetric closure of a relation
symmClose :: Ord a => Relation a -> Relation a
symmClose rel = Map.unionWith (++) rel $
                Map.fromListWith (++) [ (y,[x]) | (x,ys) <- Map.toList rel, y <- ys ]

-- | Euclidean closure of a relation
euclideanClose :: Ord a => Relation a -> Relation a
euclideanClose rel = Map.unionWith (++) rel $
                     Map.fromListWith (++)
                     [ (y,[z])
                     | (_,ys) <- Map.toList rel
                     , y <- ys
                     , z <- ys
                     ]

-- | Reflexive closure of a relation
reflClose :: Ord a => [a] -> Relation a -> Relation a
reflClose worlds rel = Map.unionWith (++) rel (Map.fromList [ (x, [x]) | x <- worlds ] )

-- | (Reflexive-)transitive closure of a relation
transClose :: Ord a => Bool -> [a] -> Relation a -> Relation a
transClose refl worlds rel = Map.fromList [ (x, Set.toList (transCloseAt refl x rel)) | x <- worlds ]

-- | A single element from the (reflexive-)transitive closure
transCloseAt :: Ord a => Bool -> a -> Relation a -> Set a
transCloseAt refl xx rel = (if refl then cons else cons') Set.empty xx
 where cons seen x
         | x `Set.member` seen = seen
         | otherwise           = cons' (Set.insert x seen) x
       cons' seen x = foldl cons seen (Map.findWithDefault [] x rel)

--------------------------------------------------------------------------------
-- * ShowS
--------------------------------------------------------------------------------

-- | 'Concatenate' a list of @ShowS@s
catShows :: [ShowS] -> ShowS
catShows = foldr (.) id

-- | Show a list by applying a function to each element
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith f = catShows . map f

-- | Show a list by applying a function to each element, and putting a separator between them
showListWithSep :: String -> (a -> ShowS) -> [a] -> ShowS
showListWithSep sep f = catShows . intersperse (showString sep) . map f

--------------------------------------------------------------------------------
-- * Lists
--------------------------------------------------------------------------------

-- | A variant of 'zipWith' that ensures the lists have the same length
zipWithSame :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithSame _ []     []     = []
zipWithSame f (x:xs) (y:ys) = f x y : zipWithSame f xs ys
zipWithSame _ _      _      = error "zipWithSame: lists have different lengths"

--------------------------------------------------------------------------------
-- * Strings
--------------------------------------------------------------------------------

-- | Escape a string for dot or json output
escapeString :: String -> String
escapeString str = '"' : esc str
  where
    esc []        = "\""
    esc ('\\':xs) = "\\\\" ++ esc xs
    esc ('"' :xs) = "\\\"" ++ esc xs
    esc ('\n':xs) = "\\n"  ++ esc xs
    esc (x   :xs) = x       : esc xs

--------------------------------------------------------------------------------
-- * Files
--------------------------------------------------------------------------------

-- | Turn a string into something that is safe to use as a filename.
--   The string will uniquely determine the filename.
toFileName :: String -> FilePath
toFileName xs = case splitAt 30 xs of
                  (ys,[]) -> toFileName' ys
                  (ys,zs) -> toFileName' ys ++ show (hash zs)
  where
    toFileName' = concatMap mkSafe
    mkSafe '.'  = "zd"
    mkSafe '_'  = "zu"
    mkSafe ' '  = "_"
    mkSafe '|'  = "zp"
    mkSafe '&'  = "ze"
    mkSafe '~'  = "zt"
    mkSafe '/'  = "zs"
    mkSafe '\\' = "zb"
    mkSafe '<'  = "zl"
    mkSafe '>'  = "zg"
    mkSafe 'z'  = "zz"
    mkSafe x | isAlphaNum x = [x]
    mkSafe _    = "z_"

--------------------------------------------------------------------------------
-- * IO Monad
--------------------------------------------------------------------------------

-- | 'error' in the IO monad
errorIO :: MonadIO m => String -> m a
errorIO e = liftIO $ throwIO $ ErrorCall e

-- | Perform two actions in parallel, use the result from the one that finishes first.
-- stolen from
-- <http://www.haskell.org/pipermail/haskell-cafe/2005-January/008314.html>
parIO :: IO a -> IO a -> IO a
parIO a1 a2 = do
  m <- newEmptyMVar
  _myThread <- myThreadId
  let handleExceptions io = io -- TODO: fix in ghc 6.10
  --let handleExceptions io = catch io $ \e -> case e of
  --            AsyncException ThreadKilled -> throwIO e          -- do let the thread be killed
  --            _                           -> throwTo myThread e -- but catch all other exceptions
  c1 <- forkIO $ handleExceptions $ putMVar m =<< a1
  c2 <- forkIO $ handleExceptions $ putMVar m =<< a2
  r <- takeMVar m
  -- killThread blocks until the thread has been killed.  Therefore, we call
  -- killThread asynchronously in case one thread is blocked in a foreign
  -- call.
  _ <- forkIO $ killThread c1 >> killThread c2
  return r

-- | Run an action with a timeout (in microseconds)
timeout :: Int -> IO a -> IO (Maybe a)
timeout n a = parIO (Just `fmap` a) (threadDelay n >> return Nothing)

-- | Run an action with a timeout (in microseconds)
timeoutWith :: Int -> IO a -> IO a -> IO a
timeoutWith n failed a = parIO a (threadDelay n >> failed)
