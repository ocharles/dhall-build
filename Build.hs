{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main where

import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( liftIO )
import Data.Foldable ( fold )
import Data.Monoid ( First(..) )
import Data.String ( fromString )
import Data.Traversable ( for )  

import Control.Monad.Trans.State.Strict ( runStateT )

import Control.Monad.State.Class ( gets )
  
import System.FilePath ( (</>), getSearchPath )

import System.Directory ( doesFileExist )

import qualified Data.Map as Map

import qualified Data.HashMap.Strict.InsOrd as InsOrd

import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText

import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck 

import qualified System.Process as Proc


main :: IO ()
main = do
  src <-
    LazyText.readFile "example.dhall" 

  parsed <-
    case Dhall.Parser.exprFromText mempty src of
      Left e ->
        throwIO e

      Right a ->
        return a

  resolved <-
    Dhall.Import.load parsed

  case Dhall.TypeCheck.typeOf resolved of
    Left e ->
      throwIO e

    Right _ ->
      return ()

  runStateT
    ( build
        ( Dhall.Core.normalize
            ( resolved
                `Expr.App` Expr.Var "build"
                `Expr.App` Expr.Var "hash-path"
                `Expr.App` Expr.Var "PATH"
                `Expr.App` Expr.Var "file"
                `Expr.App` Expr.Var "read-file"
            )
        )
    )
    ( BuildState mempty )

  return ()


type Hash = String


data BuildInputs = BuildInputs
  { biBuilders :: [String]
  , biArgs :: [String]
  }
  deriving ( Eq, Ord )


data BuildState = BuildState
  { hashes :: Map.Map BuildInputs Hash
  }


build = \case
  Expr.App ( Expr.Var "build" ) ( Expr.RecordLit fields ) -> do 
    builderExpr <-
      case InsOrd.lookup "builder" fields of
        Just builder ->
          return builder
      
        Nothing ->
          error "Missing builder"

    builderHashes <-
      liftIO ( topHashes builderExpr )

    argsExpr <-
      case InsOrd.lookup "args" fields of
        Just args ->
          return args

        Nothing ->
          error "Missing args"

    argHashes <-
      liftIO ( topHashes argsExpr )

    hashMap <-
      gets hashes

    case Map.lookup ( BuildInputs builderHashes argHashes ) hashMap of
      Just out ->
        error ( "Already built as " ++ out )

      Nothing -> do
        builder <-
          Dhall.Core.normalize <$> build builderExpr

        args <-
          Dhall.Core.normalize <$> build argsExpr

        case ( builder, args ) of
          ( Expr.TextLit ( Dhall.Core.Chunks [] builder ), Expr.ListLit _ args ) -> do
            liftIO $ print (  args )
            let
              exec =
                LazyText.unpack ( LazyBuilder.toLazyText builder ) 

              args' =
                foldMap
                  ( \( Expr.TextLit ( Dhall.Core.Chunks [] builder ) ) ->
                      [ LazyText.unpack ( LazyBuilder.toLazyText builder ) ]
                  )
                  args 

            liftIO ( Proc.callProcess exec args' )

            return "foo"

  Expr.App ( Expr.Var "PATH" ) ( Expr.TextLit ( Dhall.Core.Chunks [] builder ) ) -> do
    let
      bin =
        LazyText.unpack ( LazyBuilder.toLazyText builder )

    First abs <-
      lookupPath bin

    case abs of
      Nothing ->
        fail ( "Could not find " ++ bin ++ " in PATH" )

      Just abs ->
        return ( Expr.TextLit ( fromString abs ) )

  Expr.App ( Expr.Var "file" ) path@( Expr.TextLit ( Dhall.Core.Chunks [] builder ) ) ->
    return path

  Expr.ListLit _ elems ->
    Expr.ListLit Nothing <$> traverse build elems

  other ->
    return other


topHashes = \case 
  Expr.App ( Expr.Var "PATH" ) ( Expr.TextLit ( Dhall.Core.Chunks [] builder ) ) -> do
    let
      bin =
        LazyText.unpack ( LazyBuilder.toLazyText builder )

    First abs <-
      lookupPath bin
    
    return ( foldMap return abs )

  Expr.App ( Expr.Var "build" ) ( Expr.RecordLit fields ) -> do 
    builderHashes <- do
      builder <-
        case InsOrd.lookup "builder" fields of
          Just builder ->
            return builder

          Nothing ->
            error "Missing builder"

      liftIO ( topHashes builder )

    argHashes <- do
      args <-
        case InsOrd.lookup "args" fields of
          Just args ->
            return args

          Nothing ->
            error "Missing args"

      liftIO ( topHashes args )

    return (mappend argHashes builderHashes)

  Expr.App ( Expr.Var "file" ) ( Expr.TextLit ( Dhall.Core.Chunks [] builder ) ) ->
    return [ LazyText.unpack ( LazyBuilder.toLazyText builder ) ]

  Expr.TextLit ( Dhall.Core.Chunks [] builder ) ->
    return [ LazyText.unpack ( LazyBuilder.toLazyText builder ) ]

  Expr.ListLit _ elems ->
    fold <$> traverse topHashes elems

  other ->
    error ( "Don't know how to `hash` " ++ show ( Dhall.Pretty.prettyExpr other ) )


lookupPath bin = liftIO $ do
  searchPath <-
    getSearchPath

  fmap fold $ for searchPath $ \p -> do
    let
      abs =
        p </> bin

    exists <-
      doesFileExist abs

    return ( if exists then pure abs else mempty )
