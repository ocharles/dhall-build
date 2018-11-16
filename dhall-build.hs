{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Monad.Trans.State.Strict ( evalStateT, runStateT )
import Lens.Family
import Control.Applicative ( (<**>) )
import Control.Exception ( throwIO )
import qualified Dhall.Import

import qualified Data.Text.IO as Text
import qualified Dhall.Context
import qualified Dhall.Map
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative as OptParse

import qualified DhallBuild.DerivationTree as DhallBuild
import qualified Nix.Daemon


commandLineParser :: OptParse.ParserInfo FilePath
commandLineParser =
  OptParse.info ( parser <**> OptParse.helper ) mempty

  where

  parser =
    OptParse.strArgument ( OptParse.metavar "FILE" )


main :: IO ()
main = do
  f <-
    OptParse.execParser commandLineParser

  t <-
    Text.readFile f

  parsedExpr <-
    case Dhall.Parser.exprFromText mempty t of
      Left e ->
        throwIO e

      Right a ->
        return a

  ( res, derivationTrees ) <-
    runStateT
      ( evalStateT
        ( Dhall.Import.loadWith parsedExpr )
        ( Dhall.Import.emptyStatus "."
            & Dhall.Import.normalizer .~ Dhall.Core.ReifiedNormalizer DhallBuild.dhallBuildNormalizer
            & Dhall.Import.startingContext .~
                ( Dhall.Context.empty
                    & Dhall.Context.insert
                        "derivation"
                        ( Dhall.Core.Pi
                            "_"
                            ( Dhall.Core.Record
                                ( Dhall.Map.fromList
                                    [ ("exec", Dhall.Core.Text)
                                    , ("args", Dhall.Core.App Dhall.Core.List Dhall.Core.Text)
                                    , ("name", Dhall.Core.Text)
                                    ]
                                )
                            )
                            Dhall.Core.Text
                        )
                )
        )
      )
      []

  Nix.Daemon.withDaemon $ \nixDaemon ->
    mapM_ ( DhallBuild.addDerivationTree nixDaemon ) derivationTrees

  Text.putStrLn ( Dhall.Core.pretty res )
