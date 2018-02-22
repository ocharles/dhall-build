{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State.Class ( MonadState, modify, get )
import Control.Monad.Trans.State.Strict ( evalStateT, runStateT )
import Crypto.Hash ( SHA256, hashlazy )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import Data.Text.Lazy ( Text )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Dhall.Import ( load )
import DhallTraversal ( subExpr )
import System.Environment ( getArgs )

import qualified Data.HashMap.Strict.InsOrd as InsOrdMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..), Chunks(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Filesystem.Path.CurrentOS as Path

import qualified Nix.Daemon
import qualified Nix.Derivation
import qualified Nix.Derivations as Nix.Derivations
import qualified Nix.Instantiate
import qualified Nix.StorePath


main :: IO ()
main = do
  (f : _) <-
    getArgs

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
    exprToDerivationTree ( Dhall.Core.normalize expr )

  Nix.Daemon.withDaemon $ \nixDaemon ->
    evalStateT ( mapM_ ( addDerivationTree nixDaemon ) derivationTrees ) mempty

  LazyText.putStrLn ( Dhall.Core.pretty ( Dhall.Core.normalize res ) )


data DerivationTree
  = DerivationTree
      { dtInputs :: [DerivationTree]
      , dtExec :: Data.Text.Text
      , dtArgs :: Dhall.Vector Data.Text.Text
      , dtName :: Text
      }
  | EvalNix Text
  deriving (Show)



exprToDerivationTree
  :: Expr.Expr s Dhall.TypeCheck.X
  -> IO ( Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X , [ DerivationTree ] )
exprToDerivationTree =
  \e ->
    runStateT
      ( go
          ( Dhall.Core.normalize
              ( e
                  `Expr.App` Expr.Var "output"
              )
          )
      )
      []

  where

    go = \case
      Expr.App "output" d -> do
        this <-
          case
            Dhall.Core.normalize
              ( d
                  `Expr.App` Expr.Var "Derivation"
                  `Expr.App` Expr.Var "derive"
                  `Expr.App` Expr.Var "eval-nix"
              )
            of
            Expr.App ( Expr.Var "eval-nix" ) ( Expr.TextLit ( Expr.Chunks [] src ) ) -> do
              return ( EvalNix ( LazyBuilder.toLazyText src ) )

            Expr.App ( Expr.Var "derive" ) ( Expr.RecordLit fields ) -> do
              ( fields', inputs ) <-
                liftIO ( runStateT ( traverse go fields ) [] )

              return
                DerivationTree
                  { dtInputs = inputs
                  , dtExec =
                      fromMaybe
                        ( error $ "exec missing " <> show fields' )
                        ( InsOrdMap.lookup "exec" fields' >>= Dhall.extract Dhall.auto )
                  , dtArgs =
                      fromMaybe
                        ( error "args missing" )
                        ( InsOrdMap.lookup "arguments" fields' >>= Dhall.extract Dhall.auto )
                  , dtName =
                      fromMaybe
                        ( error "Name missing" )
                        ( InsOrdMap.lookup "name" fields' >>= Dhall.extract Dhall.auto )
                  }

        modify (this : )

        d <- liftIO $ evalStateT (derivationTreeToDerivation this) mempty

        return
          ( Nix.Derivation.outputs d
              & Map.lookup "out"
              & fromMaybe ( error "No output" )
              & Nix.Derivation.path
              & Path.encodeString
              & LazyText.pack
              & Dhall.embed Dhall.inject
          )

      e ->
        Dhall.Core.normalize <$> subExpr go e


addDerivationTree
  :: ( MonadIO m, MonadState ( Map.Map Nix.Derivation.Derivation String ) m )
  => Nix.Daemon.NixDaemon -> DerivationTree -> m ()
addDerivationTree daemon t@DerivationTree{} = do
  mapM_ ( addDerivationTree daemon ) ( dtInputs t )

  drv <-
     derivationTreeToDerivation t

  let src = LazyBuilder.toLazyText ( Nix.Derivation.buildDerivation drv )

  liftIO $ do
    LazyText.putStrLn src

    added <-
      Nix.Daemon.addTextToStore daemon ( dtName t <> ".drv" ) src []

    putStrLn $ "Added " <> LazyText.unpack added

addDerivationTree _ (EvalNix src) = do
  liftIO $ putStrLn $ "Evaluate " ++ show src


hashDerivationModulo
  :: ( MonadIO m, MonadState ( Map.Map Nix.Derivation.Derivation String) m)
  => Nix.Derivation.Derivation -> m String
hashDerivationModulo derivation = do
  hashCache <- get

  case Map.lookup derivation hashCache of
    Just cached ->
      return cached

    Nothing ->  do
      hash <-
        case Map.toList ( Nix.Derivation.outputs derivation ) of
          [ ( "out", Nix.Derivation.DerivationOutput path hashAlgo hash ) ] | not ( Data.Text.null hash ) ->
            return . show . hashlazy @SHA256 . encodeUtf8 . LazyText.fromStrict $
            "fixed:out:" <> hashAlgo <> ":" <> hash <> ":" <> fromString (Path.encodeString path)

          _ -> do
            maskedInputs <-
              fmap Map.fromList
                ( mapM
                    ( \ (path, outs) -> do
                        d <- liftIO ( Nix.Derivations.loadDerivation ( Path.encodeString path) )
                        hash <- hashDerivationModulo d
                        return ( fromString hash, outs )
                    )
                    ( Map.toList ( Nix.Derivation.inputDrvs derivation ) )
                )

            return $
              show . hashlazy @SHA256 . encodeUtf8 . LazyBuilder.toLazyText $
              Nix.Derivation.buildDerivation derivation { Nix.Derivation.inputDrvs = maskedInputs }

      modify ( Map.insert derivation hash )

      return hash


derivationTreeToDerivation
  :: ( MonadState ( Map.Map Nix.Derivation.Derivation String ) m, MonadIO m )
  => DerivationTree -> m Nix.Derivation.Derivation
derivationTreeToDerivation = \case
  EvalNix src -> do
    drvPath <-
      liftIO ( Nix.Instantiate.instantiateExpr src )

    liftIO ( Nix.Derivations.loadDerivation drvPath )

  t@DerivationTree{} -> do
    maskedInputs <-
      fmap Map.fromList
        ( mapM
            ( \t -> do
                d <- derivationTreeToDerivation t
                hash <- hashDerivationModulo d
                return ( fromString hash, Set.singleton "out" )
            )
            ( dtInputs t )
        )
    actualInputs <-
      fmap Map.fromList
        (mapM
          (\t -> do
              d <- derivationTreeToDerivation t
              path <- case t of
                EvalNix src ->
                  liftIO ( fromString <$> Nix.Instantiate.instantiateExpr src  )

                DerivationTree{} -> return $
                  fromString $ Nix.StorePath.textPath
                  (dtName t <> ".drv")
                  (LazyBuilder.toLazyText (Nix.Derivation.buildDerivation d))
              return
                ( path
                , Set.singleton "out"))
          (dtInputs t))

    let
      drv =
        Nix.Derivation.Derivation
          { Nix.Derivation.outputs =
              Map.singleton
                "out"
                (Nix.Derivation.DerivationOutput
                   (fromString (Nix.StorePath.derivationOutputPath (dtName t) drv
                               { Nix.Derivation.inputDrvs = maskedInputs }))
                   ""
                   "")
          , Nix.Derivation.inputDrvs = actualInputs
          , Nix.Derivation.inputSrcs = mempty
          , Nix.Derivation.platform = "x86_64-linux"
          , Nix.Derivation.builder = dtExec t
          , Nix.Derivation.args = dtArgs t
          , Nix.Derivation.env =
              fmap
                ( fromString . Path.encodeString . Nix.Derivation.path )
                ( Nix.Derivation.outputs drv )
          }

    return drv
