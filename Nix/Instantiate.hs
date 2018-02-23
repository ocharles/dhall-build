module Nix.Instantiate ( instantiateExpr ) where

import Data.Text.Lazy ( Text )

import qualified Data.Text.Lazy as LazyText
import qualified Data.MemoTrie as MemoTrie
import qualified System.Process as Process

import qualified MemoIO


instantiateExpr :: Text -> IO FilePath
instantiateExpr =
  MemoIO.memoIO go . LazyText.unpack

  where

  go src =
    init
      <$>
        Process.readCreateProcess
          ( Process.proc "nix-instantiate" [ "-E", src ] )
          ""
