{-|
Stability: experimtal

This module is experimental, and its API might change between point releases. Use at your own risk.
-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase            #-}

module Apecs.Experimental.Stores
  ( Pushdown(..), Stack(..), PriorityMap(..), LowestPriority(..)
  ) where


import           Data.IORef
import Control.Monad.Reader
import Data.Proxy
import Data.Semigroup
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector.Unboxed         as U
import           GHC.TypeLits
import           Data.Typeable (Typeable, typeRep)

import Apecs.Components (MaybeStore (..))
import Apecs.Core

newtype LowestPriority c = LowestPriority (PriorityMap c)
type instance Elem (LowestPriority c) = c

instance (Functor m, ExplInit m (PriorityMap c)) => ExplInit m (LowestPriority c) where
  explInit = LowestPriority <$> explInit

instance (Monad m, ExplGet m (PriorityMap c), Elem (PriorityMap c) ~ Elem (LowestPriority c)) =>
  ExplGet m (LowestPriority c) where
    explExists (LowestPriority s) = explExists s 
    explGet (LowestPriority s) = explGet s 

instance (Monad m, ExplGet m (PriorityMap c), ExplSet m (PriorityMap c), Elem (PriorityMap c) ~ Elem (LowestPriority c)) =>
  ExplSet m (LowestPriority c) where
    explSet (LowestPriority s) = explSet s

instance (Monad m, 
          ExplGet m (PriorityMap c), 
          ExplSet m (PriorityMap c), 
          ExplDestroy m (PriorityMap c), 
          Elem (PriorityMap c) ~ Elem (LowestPriority c)) =>
  ExplDestroy m (LowestPriority c) where
    explDestroy (LowestPriority s) = explDestroy s

instance
  ( Monad m
  , ExplMembers m (PriorityMap c)
  , Elem (PriorityMap c) ~ Elem (LowestPriority c)) => ExplMembers m (LowestPriority c) where
    explMembers (LowestPriority s) = U.take 1 <$> explMembers s

-- | A priority map based on @Data.HashPSQ@. Members returned least p to larget p. Worst case of O(min(n, Int width)) for most operations.
newtype PriorityMap c = PriorityMap (IORef (PSQ.IntPSQ c ()))

type instance Elem (PriorityMap c) = c
instance MonadIO m => ExplInit m (PriorityMap c) where
  explInit = liftIO$ PriorityMap <$> newIORef PSQ.empty

instance (MonadIO m, Typeable c, Ord c) => ExplGet m (PriorityMap c) where
  explExists (PriorityMap ref) ety = liftIO$ PSQ.member ety <$> readIORef ref
  explGet    (PriorityMap ref) ety = liftIO$ flip fmap (PSQ.lookup ety <$> readIORef ref) $ \case
    Just (c, _) -> c
    notFound -> error $ unwords
      [ "Reading non-existent PriorityMap component"
      , show (typeRep notFound)
      , "for entity"
      , show ety
      ]
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance (Ord c, MonadIO m) => ExplSet m (PriorityMap c) where
  {-# INLINE explSet #-}
  explSet (PriorityMap ref) ety x = liftIO$
    modifyIORef' ref (PSQ.insert ety x ())

instance (Ord c, MonadIO m) => ExplDestroy m (PriorityMap c) where
  {-# INLINE explDestroy #-}
  explDestroy (PriorityMap ref) ety = liftIO$
    readIORef ref >>= writeIORef ref . PSQ.delete ety

instance (MonadIO m) => ExplMembers m (PriorityMap c) where
  {-# INLINE explMembers #-}
  explMembers (PriorityMap ref) = liftIO$ U.fromList . PSQ.keys <$> readIORef ref


-- | Overrides a store to have history/pushdown semantics.
--   Setting this store adds a new value on top of the stack.
--   Destroying pops the stack.
--   You can view the entire stack using the 'Stack' wrapper.
newtype Pushdown s c = Pushdown (s (Stack c))
newtype Stack c = Stack {getStack :: [c]} deriving (Eq, Show, Functor, Applicative, Monad, Foldable, Monoid, Semigroup)

type instance Elem (Pushdown s c) = c

instance (Functor m, ExplInit m (s (Stack c))) => ExplInit m (Pushdown s c) where
  explInit = Pushdown <$> explInit

pattern StackList :: c -> [c] -> Maybe (Stack c)
pattern StackList x xs = Just (Stack (x:xs))

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplGet m (Pushdown s c) where
    explExists (Pushdown s) ety = f <$> explGet (MaybeStore s) ety
      where
        f (StackList _ _) = True
        f _               = False
    explGet (Pushdown s) ety = head . getStack <$> explGet s ety

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , ExplSet m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplSet m (Pushdown s c) where
    explSet (Pushdown s) ety c = do
      ms <- explGet (MaybeStore s) ety
      let tail (StackList _ cs) = cs
          tail _                = []
      explSet s ety (Stack (c:tail ms))

instance
  ( Monad m
  , ExplGet m (s (Stack c))
  , ExplSet m (s (Stack c))
  , ExplDestroy m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplDestroy m (Pushdown s c) where
    explDestroy (Pushdown s) ety = do
      mscs <- explGet (MaybeStore s) ety
      case mscs of
        StackList _ cs' -> explSet s ety (Stack cs')
        _               -> explDestroy s ety

instance
  ( Monad m
  , ExplMembers m (s (Stack c))
  , Elem (s (Stack c)) ~ Stack c
  ) => ExplMembers m (Pushdown s c) where
    explMembers (Pushdown s) = explMembers s

instance (Storage c ~ Pushdown s c, Component c) => Component (Stack c) where
  type Storage (Stack c) = StackStore (Storage c)

newtype StackStore s = StackStore s
type instance Elem (StackStore s) = Stack (Elem s)

instance (Storage c ~ Pushdown s c, Has w m c) => Has w m (Stack c) where
  getStore = StackStore <$> getStore

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplGet m (s (Stack c))
  ) => ExplGet m (StackStore (Pushdown s c)) where
  explExists (StackStore s) = explExists s
  explGet (StackStore (Pushdown s)) = explGet s

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplSet     m (s (Stack c))
  , ExplDestroy m (s (Stack c))
  ) => ExplSet m (StackStore (Pushdown s c)) where
  explSet (StackStore (Pushdown s)) ety (Stack []) = explDestroy s ety
  explSet (StackStore (Pushdown s)) ety st         = explSet s ety st

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplDestroy m (s (Stack c))
  ) => ExplDestroy m (StackStore (Pushdown s c)) where
  explDestroy (StackStore (Pushdown s)) = explDestroy s

instance
  ( Elem (s (Stack c)) ~ Stack c
  , ExplMembers m (s (Stack c))
  ) => ExplMembers m (StackStore (Pushdown s c)) where
  explMembers (StackStore (Pushdown s)) = explMembers s
