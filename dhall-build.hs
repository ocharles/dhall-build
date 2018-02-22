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

import qualified Dhall.Parser
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.TypeCheck
import qualified Filesystem.Path.CurrentOS as Path

import qualified Nix.Daemon
import qualified Nix.Derivation as Nix
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


data DerivationTree = DerivationTree
  { dtInputs :: [DerivationTree]
  , dtExec :: Data.Text.Text
  , dtArgs :: Dhall.Vector Data.Text.Text
  , dtName :: Text
  } | EvalNix Text deriving (Show)



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
            Expr.App ( Expr.Var "eval-nix" ) ( Expr.TextLit src ) -> do
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
                        ( Map.lookup "exec" fields' >>= Dhall.extract Dhall.auto )
                  , dtArgs =
                      fromMaybe
                        ( error "args missing" )
                        ( Map.lookup "arguments" fields' >>= Dhall.extract Dhall.auto )
                  , dtName =
                      fromMaybe
                        ( error "Name missing" )
                        ( Map.lookup "name" fields' >>= Dhall.extract Dhall.auto )
                  }

        modify (this : )

        d <- liftIO $ evalStateT (derivationTreeToDerivation this) mempty

        return
          ( Nix.outputs d
              & Map.lookup "out"
              & fromMaybe ( error "No output" )
              & Nix.path
              & Path.encodeString
              & LazyText.pack
              & Dhall.embed Dhall.inject
          )

      e ->
        Dhall.Core.normalize <$> subExpr go e


addDerivationTree
  :: ( MonadIO m, MonadState ( Map.Map Nix.Derivation String ) m )
  => Nix.Daemon.NixDaemon -> DerivationTree -> m ()
addDerivationTree daemon t@DerivationTree{} = do
  mapM_ ( addDerivationTree daemon ) ( dtInputs t )

  drv <-
     derivationTreeToDerivation t

  let src = LazyBuilder.toLazyText ( Nix.buildDerivation drv )

  liftIO $ do
    LazyText.putStrLn src

    added <-
      Nix.Daemon.addTextToStore daemon ( dtName t <> ".drv" ) src []

    putStrLn $ "Added " <> LazyText.unpack added

addDerivationTree _ (EvalNix src) = do
  liftIO $ putStrLn $ "Evaluate " ++ show src


loadDerivation :: FilePath -> IO Nix.Derivation
loadDerivation drvPath = do
  drvText <-
    LazyText.readFile drvPath

  let
    Attoparsec.Done _ result =
      Attoparsec.parse Nix.parseDerivation drvText

  return result


hashDerivationModulo
  :: ( MonadIO m, MonadState ( Map.Map Nix.Derivation String) m)
  => Nix.Derivation -> m String
hashDerivationModulo derivation = do
  hashCache <- get

  case Map.lookup derivation hashCache of
    Just cached ->
      return cached

    Nothing ->  do
      hash <-
        case Map.toList ( Nix.outputs derivation ) of
          [ ( "out", Nix.DerivationOutput path hashAlgo hash ) ] | not ( Data.Text.null hash ) ->
            return . show . hashlazy @SHA256 . encodeUtf8 . LazyText.fromStrict $
            "fixed:out:" <> hashAlgo <> ":" <> hash <> ":" <> fromString (Path.encodeString path)

          _ -> do
            maskedInputs <-
              fmap Map.fromList
                ( mapM
                    ( \ (path, outs) -> do
                        d <- liftIO ( loadDerivation ( Path.encodeString path) )
                        hash <- hashDerivationModulo d
                        return ( fromString hash, outs )
                    )
                    ( Map.toList ( Nix.inputDrvs derivation ) )
                )

            return $
              show . hashlazy @SHA256 . encodeUtf8 . LazyBuilder.toLazyText $
              Nix.buildDerivation derivation { Nix.inputDrvs = maskedInputs }

      modify ( Map.insert derivation hash )

      return hash


derivationTreeToDerivation
  :: ( MonadState ( Map.Map Nix.Derivation String ) m, MonadIO m )
  => DerivationTree -> m Nix.Derivation
derivationTreeToDerivation = \case
  EvalNix src -> do
    drvPath <-
      liftIO ( Nix.Instantiate.instantiateExpr src )

    liftIO ( loadDerivation drvPath )

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
                  (LazyBuilder.toLazyText (Nix.buildDerivation d))
              return
                ( path
                , Set.singleton "out"))
          (dtInputs t))

    let
      drv =
        Nix.Derivation
          { Nix.outputs =
              Map.singleton
                "out"
                (Nix.DerivationOutput
                   (fromString (Nix.StorePath.derivationOutputPath (dtName t) drv
                               { Nix.inputDrvs = maskedInputs }))
                   ""
                   "")
          , Nix.inputDrvs = actualInputs
          , Nix.inputSrcs = mempty
          , Nix.platform = "x86_64-linux"
          , Nix.builder = dtExec t
          , Nix.args = dtArgs t
          , Nix.env =
              fmap (fromString . Path.encodeString . Nix.path) (Nix.outputs drv)
          }

    return drv
