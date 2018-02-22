module Nix.Instantiate ( instantiateExpr ) where

import Data.Text.Lazy ( Text )

import qualified Data.Text.Lazy as LazyText
import qualified System.Process as Process

instantiateExpr :: Text -> IO FilePath
instantiateExpr src =
  init
    <$>
      Process.readCreateProcess
        ( Process.proc "nix-instantiate" [ "-E", LazyText.unpack src ] )
        ""
