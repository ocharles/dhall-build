{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Derivations ( loadDerivation ) where

import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import qualified Data.Text.Lazy.IO as LazyText
import qualified Nix.Derivation as Nix

loadDerivation :: FilePath -> IO Nix.Derivation
loadDerivation drvPath = do
  drvText <-
    LazyText.readFile drvPath

  let
    Attoparsec.Done _ result =
      Attoparsec.parse Nix.parseDerivation drvText

  return result
