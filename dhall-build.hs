{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Control.Applicative ( (<**>) )
import Control.Exception ( throwIO )
import Dhall.Import ( load )

import qualified Data.Text.Lazy.IO as LazyText
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
    LazyText.readFile f

  parsedExpr <-
    case Dhall.Parser.exprFromText mempty t of
      Left e ->
        throwIO e

      Right a ->
        return a

  expr <-
    Dhall.Import.load parsedExpr

  _ <-
    case Dhall.TypeCheck.typeOf expr of
      Left e ->
        throwIO e

      Right a ->
        return a

  ( res, derivationTrees ) <-
    DhallBuild.exprToDerivationTree ( Dhall.Core.normalize expr )

  Nix.Daemon.withDaemon $ \nixDaemon ->
    mapM_ ( DhallBuild.addDerivationTree nixDaemon ) derivationTrees

  LazyText.putStrLn ( Dhall.Core.pretty ( Dhall.Core.normalize res ) )
