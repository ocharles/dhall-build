{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

  evalStateT ( mapM_ addDerivationTree derivationTrees ) mempty

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


addDerivationTree t@DerivationTree{} = do
  mapM_ addDerivationTree ( dtInputs t )

  drv <-
     derivationTreeToDerivation t

  let src = LazyBuilder.toLazyText ( Nix.buildDerivation drv )

  liftIO $ do
    LazyText.putStrLn src

    added <-
      addTextToStore ( dtName t <> ".drv" ) src []

    putStrLn $ "Added " <> LazyText.unpack added

addDerivationTree (EvalNix src) = do
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
                fromString $ makeTextPath
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
                 (fromString (derivationOutput (dtName t) drv
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

addTextToStore name text references = do
  s <- N.socket N.AF_UNIX N.Stream 0
  N.connect s (N.SockAddrUnix "/nix/var/nix/daemon-socket/socket")

  writeInt s 1852405859

  _WORKER_MAGIC_2 <- N.recv s 8
  _PROTOCOL_VERSION <- N.recv s 8

  writeInt s 0 -- Padding?

  writeInt s 8 -- wopAddTextToStore

  writeText s name
  writeText s text
  writeTexts s references

  readNum s
  readNum s
  readText s

writeInt s n =
  N.sendAll s ( BS.toLazyByteString ( BS.int64LE n ) ) -- wopAddTextToStore 

writeText s text = do
  N.sendAll s ( BS.toLazyByteString ( BS.int64LE ( L.length text ) ) )
  N.sendAll s ( L.encodeUtf8 text )
  case L.length text `mod` 8 of
    n | n > 0 ->
        N.sendAll s ( BS.replicate ( 8 - n ) 0 )
    _ -> return ()

writeTexts s texts = do
  writeInt s ( fromIntegral ( length texts ) )
  mapM_ (writeText s) texts

readNum s = do
  Binary.runGet Binary.getInt64le <$> N.recv s 8

readText s = do
  len <- readNum s
  t <- L.decodeUtf8 <$> N.recv s ( fromIntegral len )
  case len `mod` 8 of
    n | n > 0 ->
        N.recv s ( 8 - n ) >> return ()
    _ -> return ()
  return t
  

-- 
-- 
-- instantiate :: Expr s Op -> WriterT [DerivationTree] IO (Expr X X)
-- instantiate e =
--   case normalize e of
--     App (App (Embed OpDerive) name) script -> do
--       (script', inputs) <- lift (runWriterT (instantiate script))
--       let tree =
--             DerivationTree
--             { dtInputs = inputs
--             , dtScript =
--                 case normalize script' of
--                   TextLit builder -> LazyBuilder.toLazyText builder
--             , dtName =
--                 case normalize name of
--                   TextLit builder -> LazyBuilder.toLazyText builder
--             }
--       liftIO $ putStrLn "dtScript:"
--       liftIO (print (dtScript tree))
--       tell [tree]
--       return
--         (TextLit
--            (fromString . show . Nix.path $
--             (Nix.outputs (derivationTreeDerivation tree) Map.! "out")))
--     TextLit t -> return (TextLit t)
--     TextAppend l r -> do
--       l <- instantiate l
--       r <- instantiate r
--       return (TextAppend l r)

-- bsChunks :: Int -> ByteString -> [ByteString]
bsChunks n bytes =
  if BS.length bytes > n
    then BS.take n bytes : bsChunks n (BS.drop n bytes)
    else [bytes]

-- compressHash :: ByteString -> ByteString
compressHash hash =
  foldl
    (\hash' chunk -> BS.pack (BS.zipWith xor hash' chunk))
    (BS.replicate 20 0)
    (map (\c -> BS.append c (BS.take (20 - BS.length c) (BS.replicate 20 0))) $
     bsChunks 20 hash)

context = empty & insert "derive" (Pi "_" Text (Pi "_" Text Text))

base32 bytes =
  map
    (\n ->
       let b = n * 5
       in let i = b `div` 8
          in let j = b `mod` 8
             in let c =
                      ((BS.head (BS.drop (fromIntegral i) bytes)) `shiftR` j) .|.
                      (if fromIntegral i >= BS.length bytes - 1
                         then 0
                         else (BS.head (BS.drop (fromIntegral (i + 1)) bytes) `shiftL`
                               (8 - j)))
                in alphabet !! fromIntegral (c .&. 0x1f))
    (reverse [0 .. 32 - 1 :: Int])
  where
    alphabet = "0123456789abcdfghijklmnpqrsvwxyz"

derivationOutput derivationName derivation =
  derivation & clearOutputs & hashDerivation & encodeUtf8 .
  formatOutputDescription &
  makeOutputHash
  where
    hashDerivation =
      T.pack . show . hashlazy @SHA256 . encodeUtf8 . LazyBuilder.toLazyText .
      Nix.buildDerivation
    clearOutputs drv =
      drv
      { Nix.outputs = Nix.DerivationOutput "" "" "" <$ Nix.outputs drv
      , Nix.env = Map.union ("" <$ Nix.outputs drv) (Nix.env drv)
      }
    formatOutputDescription derivationHash =
      "output:out:sha256:" <> derivationHash <> ":" <> nixStorePath <> ":" <>
      derivationName
    humanName = derivationName
    makeOutputHash str =
      let hashPart =
            str & hashlazy @SHA256 & compressHash . BS.pack . Mem.unpack &
            base32
      in T.unpack nixStorePath <> "/" <> hashPart <> "-" <> T.unpack humanName
    nixStorePath = "/nix/store"

makeTextPath derivationName text =
  text & hashText & encodeUtf8 . formatOutputDescription & makeOutputHash
  where
    hashText = T.pack . show . hashlazy @SHA256 . encodeUtf8
    formatOutputDescription textHash =
      "text:sha256" <> ":" <> textHash <> ":" <> nixStorePath <> ":" <> derivationName
    humanName = derivationName
    makeOutputHash str =
      let hashPart =
            str
              & hashlazy @SHA256
              & compressHash . BS.pack . Mem.unpack
              & base32
      in T.unpack nixStorePath <> "/" <> hashPart <> "-" <> T.unpack humanName
    nixStorePath = "/nix/store"


