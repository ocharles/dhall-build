{-# language TypeOperators #-}

-- | Provide a cache for idempotent IO operations.

module MemoIO ( memoIO, memoIOWith ) where

import System.IO.Unsafe

import qualified Data.MemoTrie as MemoTrie
import qualified System.IO.Unsafe


memoIO :: MemoTrie.HasTrie a => ( a -> IO b ) -> ( a -> IO b )
memoIO io =
  memoIOWith MemoTrie.trie MemoTrie.untrie io

memoIOWith
  :: ( ( a -> b ) -> ( a MemoTrie.:->: b ) )
  -> ( ( a MemoTrie.:->: b ) -> ( a -> b ) )
  -> ( a -> IO b )
  -> ( a -> IO b )
memoIOWith trie untrie io =
  return <$> untrie ( trie ( System.IO.Unsafe.unsafePerformIO . io ) )
