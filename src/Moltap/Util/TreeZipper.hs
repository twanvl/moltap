{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Util.TreeZipper
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A labeled tree data type, and a zipper over that type.
--
--------------------------------------------------------------------------------

module Moltap.Util.TreeZipper
    ( -- * Trees
      Tree(..), LabeledForest, LabeledTree(..)
    , toGraph, singleton
      -- * Tree zipper
    , TreeZipper
    , toTree, fromTree, fromRoot
      -- * Properties
    , isRoot, getValue, getLabel, setValue, setLabel
      -- * Movement
    , up, downNew, downFirst, next, root
    , NodeName, name, downTo
    ) where

import qualified Data.Sequence as S
import Data.Sequence (Seq, (><), (<|), ViewL(..), ViewR(..))
import qualified Data.Foldable as F
import Data.Traversable hiding (mapAccumL)
import Data.List
import Control.Applicative

#ifdef TEST
import Test.QuickCheck
#endif

--------------------------------------------------------------------------------
-- Tree type
--------------------------------------------------------------------------------

-- | A tree. Edges are labled with type @l@ and nodes store values of type @a@
data Tree l a = Branch
    { branchValue :: a
    , subTree     :: LabeledForest l a
    }
    deriving (Eq, Ord)

type LabeledForest l a = Seq (LabeledTree l a)

data LabeledTree l a = Edge l (Tree l a)
    deriving (Eq, Ord)

-- | A singleton tree
singleton :: a -> Tree l a
singleton a = Branch a S.empty

-- | Convert a tree to a graph.
--
--   A graph consists of nodes (named with integers) and outgoing edges for each node.
--   The root will have key 0.
toGraph :: Tree l a -> [(Int,a,[(Int,l)])]
toGraph = snd . treeToGraph 0
 where treeToGraph :: Int -> Tree l a -> (Int, [(Int,a,[(Int,l)])])
       treeToGraph i (Branch b s)
         = i `seq`
           let (i',nes) = mapAccumL edgeToGraph (succ i) (F.toList s)
               (nodes,edges) = unzip nes
           in (i', (i,b,edges):concat nodes)
       edgeToGraph i (Edge l t)
         = i `seq`
           let (i',nodes) = treeToGraph i t
           in  (i',(nodes,(i,l)))

--------------------------------------------------------------------------------
-- Tree zipper
--------------------------------------------------------------------------------

-- | A zipper in a labeled tree.
--   Maintains a current node, from which we can move in various directions.
data TreeZipper l a = TreeZipper (TreePath l a) (Tree l a)
    deriving (Eq)

-- | A path down a tree
data TreePath l a
    = Root
    | Down (LabeledForest l a) l a (LabeledForest l a) (TreePath l a)
    deriving (Eq)

-- | Construct a TreeZipper from a Tree
fromTree :: Tree l a -> TreeZipper l a
fromTree t = TreeZipper Root t

-- | Extract a tree from a zipper.
--   For a zipper not at the root returns the subtree for the current node.
toTree :: TreeZipper l a -> Tree l a
toTree (TreeZipper _ t) = t

-- | A singleton TreeZipper
fromRoot :: a -> TreeZipper l a
fromRoot = fromTree . singleton

-- | Is a zipper at the root?
isRoot :: TreeZipper l a -> Bool
isRoot (TreeZipper Root _) = True
isRoot _                   = False

-- | Get the value of the current node
getValue :: TreeZipper l a -> a
getValue = branchValue . toTree

-- | Get the label of the current node, can not be used at the root
getLabel :: TreeZipper l a -> l
getLabel (TreeZipper Root             _) = error "Zipper at root of tree"
getLabel (TreeZipper (Down _ l _ _ _) _) = l

-- | Set the value of the current node
setValue :: a -> TreeZipper l a -> TreeZipper l a
setValue a' (TreeZipper path (Branch _ t)) = TreeZipper path (Branch a' t)

-- | Set the label of the current node, can not be used at the root
setLabel :: l -> TreeZipper l a -> TreeZipper l a
setLabel _  (TreeZipper Root             _) = error "Zipper at root of tree"
setLabel l' (TreeZipper (Down x _ a y p) t) = (TreeZipper (Down x l' a y p) t)

--------------------------------------------------------------------------------
-- Movement
--------------------------------------------------------------------------------

-- | Move up the tree, fails if the zipper is at the root
up :: TreeZipper l a -> TreeZipper l a
up (TreeZipper Root                _) = error "Zipper at root of tree"
up (TreeZipper (Down x l a y path) t) = TreeZipper path $ Branch a (x >< S.singleton (Edge l t) >< y)

-- | Move to the root of a zipper
root :: TreeZipper l a -> TreeZipper l a
root z
 | isRoot z  = z
 | otherwise = root (up z)

-- | Move down the tree, constructing a new node
downNew :: l -> a -> TreeZipper l a -> TreeZipper l a
downNew l a (TreeZipper path (Branch b t)) = TreeZipper (Down t l b S.empty path) (Branch a S.empty)

-- | Move down the tree, to the first (rightmost) node
downFirst :: TreeZipper l a -> Maybe (TreeZipper l a)
downFirst (TreeZipper path (Branch b t)) = case S.viewr t of
        EmptyR         -> fail "No children"
        x :> Edge l t' -> return $ TreeZipper (Down x l b S.empty path) t'

-- | Move to the next node, if there is any
next :: TreeZipper l a -> Maybe (TreeZipper l a)
next (TreeZipper Root                _) = fail "Zipper at root of tree"
next (TreeZipper (Down x l a y path) t) = case S.viewr x of
        EmptyR           -> fail "at the end"
        x' :> Edge l' t' -> return $ TreeZipper (Down x' l' a (Edge l t <| y) path) t'

--------------------------------------------------------------------------------
-- Named nodes
--------------------------------------------------------------------------------

-- | The name of a node, after moving up the tree it is possible to move down to this node again
newtype NodeName = NN Int

-- | Name of the current position in the tree. Not valid at the root
name :: TreeZipper l a -> NodeName
name (TreeZipper Root             _) = error "Zipper at root of tree"
name (TreeZipper (Down x _ _ _ _) _) = NN (S.length x)

-- | Move down the tree to a node whos name was queried before moving up
downTo :: NodeName -> TreeZipper l a -> TreeZipper l a
downTo (NN i) (TreeZipper path (Branch b t)) = TreeZipper (Down before l b after path) n
  where (before,n_and_after) = S.splitAt i t
        (Edge l n :< after) = S.viewl n_and_after

--------------------------------------------------------------------------------
-- Functor instances
--------------------------------------------------------------------------------

instance Functor (Tree l) where
    fmap f (Branch a s) = Branch (f a) (fmap (fmap f) s)
instance Functor (LabeledTree l) where
    fmap f (Edge l t) = Edge l (fmap f t)

instance Functor (TreeZipper l) where
    fmap f (TreeZipper p t) = TreeZipper (fmap f p) (fmap f t)
instance Functor (TreePath l) where
    fmap _ Root             = Root
    fmap f (Down x l a y p) = Down (fmap (fmap f) x) l (f a) (fmap (fmap f) y) (fmap f p)

instance F.Foldable (Tree l) where
    foldr f z (Branch a s) = f a (F.foldr (\lt y -> F.foldr f y lt) z s)
instance F.Foldable (LabeledTree l) where
    foldr f z (Edge   _ t) = F.foldr f z t

instance Traversable (Tree l) where
    traverse f (Branch a s) = Branch <$> f a <*> traverse (traverse f) s
instance Traversable (LabeledTree l) where
    traverse f (Edge   l t) = Edge l <$> traverse f t

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance (Show l, Show a) => Show (Tree l a) where
    showsPrec p = showParen (p > 0) . showTree "    "

showTree :: (Show l, Show a) => String -> Tree l a -> ShowS
showTree ind (Branch a s)
    = shows a . showString "\n"
    . foldr (.) id [ showString ind . shows l . showString " -> " . showTree ("    "++ind) t
                   | Edge l t <- F.toList s ]

instance (Show l, Show a) => Show (TreeZipper l a) where
    showsPrec p = showsPrec p . toTree . root . markHere

-- | A type to mark a specific position in a tree
data Here a = Here a | NotHere a

instance Show a => Show (Here a) where
    showsPrec _ (Here a)    = showString "[ " . showsPrec 0 a . showString " ]"
    showsPrec p (NotHere a) = showsPrec p a

-- | Mark the current position in the zipper
markHere :: TreeZipper l a -> TreeZipper l (Here a)
markHere (TreeZipper p (Branch a sub))
   = TreeZipper (fmap NotHere p) (Branch (Here a) (fmap (fmap NotHere) sub))

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

#ifdef TEST

instance Arbitrary a => Arbitrary (Seq a) where
    arbitrary   = fmap S.fromList arbitrary
    coarbitrary = coarbitrary . F.toList

instance (Arbitrary a, Arbitrary l) => Arbitrary (Tree l a) where
    arbitrary = sized $ \n -> do
        a    <- arbitrary
        down <- resize (n `div` 3) arbitrary
        return (Branch a down)
    coarbitrary (Branch a s) = coarbitrary a . coarbitrary s
instance (Arbitrary a, Arbitrary l) => Arbitrary (LabeledTree l a) where
    arbitrary = do
        l <- arbitrary
        t <- arbitrary
        return (Edge l t)
    coarbitrary (Edge l t) = coarbitrary l . coarbitrary t

instance (Arbitrary a, Arbitrary l) => Arbitrary (TreePath l a) where
    arbitrary = do
        steps <- arbitrary
        return $ foldr down Root steps
      where down (a,b,c,d) e = Down a b c d e
    coarbitrary Root             = variant 0
    coarbitrary (Down x l a y p) = variant 1 . coarbitrary (x,(l,a),y,p)
instance (Arbitrary a, Arbitrary l) => Arbitrary (TreeZipper l a) where
    arbitrary = do
        l <- arbitrary
        p <- arbitrary
        return (TreeZipper l p)
    coarbitrary (TreeZipper l p) = coarbitrary l . coarbitrary p

#endif
