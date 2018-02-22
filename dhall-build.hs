{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Nix.Daemon
import qualified Nix.StorePath


import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Crypto.Hash
import Data.Bits
import Data.Bits (xor)
import qualified Data.ByteArray as Mem
import qualified Data.ByteString.Lazy as BS
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.IO (readFile)
import qualified Data.Text.Lazy.IO as T
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Context (empty, insert)
import Dhall.Core
  ( Expr(..)
  , Normalizer
  , Var(V)
  , normalize
  , normalizeWith
  , pretty
  , subst
  )
import Dhall.Import (load)
import Dhall.Parser (exprFromText)
import Dhall.TypeCheck (X, absurd, typeWith)
import qualified Filesystem.Path.CurrentOS as Path
import qualified Nix.Derivation as Nix
import Prelude hiding (readFile)

import Control.Exception ( throwIO )
import Control.Monad.Trans.State.Strict ( execState, runStateT, evalStateT )
import Control.Monad.State.Class ( modify, get )

import Data.Maybe ( fromMaybe )

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

import qualified Network.Socket as N hiding ( recv )
import qualified Network.Socket.ByteString.Lazy as N

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy.Builder as BS

import Data.Int
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import Debug.Trace

import DhallTraversal
import qualified System.Process as Process
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import qualified Data.Text

import System.Environment  

data Derive = Derive
  { drvScript :: String
  }

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

  exprType <-
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



exprToDerivationTree e =
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
      e@( Expr.App "output" d ) -> do
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
        normalize <$> subExpr go e


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

loadDerivation drvPath = do 
  drvText <-
    LazyText.readFile drvPath

  let Attoparsec.Done _ result = Attoparsec.parse Nix.parseDerivation drvText

  return result

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

evalNix src =
  init <$> Process.readCreateProcess ( Process.proc "nix-instantiate" [ "-E", LazyText.unpack src ] ) ""
  

derivationTreeToDerivation (EvalNix src) = do
  drvPath <-
    liftIO ( evalNix src )

  liftIO ( loadDerivation drvPath )

derivationTreeToDerivation t@DerivationTree{} = do
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
                liftIO ( fromString <$> evalNix src  )

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




context = empty & insert "derive" (Pi "_" Text (Pi "_" Text Text))
