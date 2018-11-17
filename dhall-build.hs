{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Dhall.Core ( Expr(..) )
import qualified Dhall.Map as Map
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
            & Dhall.Import.startingContext .~ context
        )
      )
      []

  mapM_ print derivationTrees

  Nix.Daemon.withDaemon $ \nixDaemon ->
    mapM_ ( DhallBuild.addDerivationTree nixDaemon ) derivationTrees

  Text.putStrLn ( Dhall.Core.pretty res )


-- context :: Dhall.Context.Context (Expr s X)
context =
  Dhall.Context.insert
    "derivation"
    ( Pi
        "_"
        ( Record
            ( Map.fromList
                [ ( "args", List `App` Text )
                , ( "builder"
                  , Union
                      ( Map.fromList
                          [ ( "Builtin"
                            , Union
                                ( Map.fromList
                                    [ ( "Fetch-Url", Record mempty ) ]
                                )
                            )
                          , ( "Exe", Text )
                          ]
                      )
                  )
                , ( "environment"
                  , List
                      `App`
                        Record
                          ( Map.fromList
                              [ ( "name", Text )
                              , ( "value"
                                , Union
                                    ( Map.fromList
                                        [ ( "Bool", Bool ), ( "Text", Text ) ]
                                    )
                                )
                              ]
                          )
                  )
                , ( "name", Text )
                , ( "output-hash"
                  , Optional
                      `App`
                        Record
                          ( Map.fromList
                              [ ("algorithm"
                                , Union
                                    ( Map.fromList
                                        [ ( "SHA256", Record mempty ) ]
                                    )
                                )
                              , ( "hash", Text )
                              , ( "mode"
                                , Union
                                    ( Map.fromList
                                        [ ("Flat", Record mempty)
                                        , ("Recursive", Record mempty)
                                        ]
                                    )
                                )
                              ]
                          )
                  )
                , ( "outputs", List `App` Text )
                , ( "system"
                  , Union
                      ( Map.fromList
                          [ ( "builtin", Record mempty )
                          , ( "x86_64-linux", Record mempty )
                          ]
                      )
                  )
                ]
            )
        )
        Text
    )
    Dhall.Context.empty
