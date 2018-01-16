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

data Op =
  OpDerive
  deriving (Show)

instance Pretty Op

data Derive = Derive
  { drvScript :: String
  }

go = do
  t <- readFile "basic.dhall"
  expr <-
    case exprFromText mempty t of
      Left e -> error (show e)
      Right a -> return a
  expr <- load expr
  exprType <-
    case typeWith context expr of
      Left e -> error (show e)
      Right a -> return a
  putStrLn "Type:"
  T.putStrLn (pretty (normalize exprType `asTypeOf` exprType))
  putStrLn ""
  putStrLn "Expr:"
  expr <-
    return
      (subst (V "derive" 0) (Embed OpDerive) (fmap absurd expr) :: Expr () Op)
  print expr
  T.putStrLn (pretty expr)
  (expr, dtree) <- runWriterT (instantiate expr)
  putStrLn "After substitution:"
  print expr
  T.putStrLn (pretty expr)
  print dtree
  mapM_
    (T.putStrLn . LazyBuilder.toLazyText . Nix.buildDerivation)
    (concatMap flatten dtree)

flatten :: DerivationTree -> [Nix.Derivation]
flatten t = derivationTreeDerivation t : concatMap flatten (dtInputs t)

derivationTreeDerivation :: DerivationTree -> Nix.Derivation
derivationTreeDerivation t =
  let drv =
        Nix.Derivation
        { Nix.outputs =
            Map.singleton
              "out"
              (Nix.DerivationOutput
                 (fromString (derivationOutput (dtName t) drv))
                 ""
                 "")
        , Nix.inputDrvs =
            Map.fromList
              (map
                 (\t ->
                    ( fromString (T.unpack ("??" <> ".drv"))
                    , Set.singleton "out"))
                 (dtInputs t))
        , Nix.inputSrcs = mempty
        , Nix.platform = "x86_64-linux"
        , Nix.builder =
            "/nix/store/hqi64wjn83nw4mnf9a5z9r4vmpl72j3r-bash-4.4-p12/bin/bash"
        , Nix.args = pure "/nix/store/cqrm2989cn9109dsf3nryaffr3lfd2nr-builder" -- pure (fromString (show (hashlazy @SHA256 (encodeUtf8 (dtScript t)))))
        , Nix.env =
            fmap (fromString . Path.encodeString . Nix.path) (Nix.outputs drv)
        }
  in drv

data DerivationTree = DerivationTree
  { dtInputs :: [DerivationTree]
  , dtScript :: Text
  , dtName :: Text
  } deriving (Show)

instantiate :: Expr s Op -> WriterT [DerivationTree] IO (Expr X X)
instantiate e =
  case normalize e of
    App (App (Embed OpDerive) name) script -> do
      (script', inputs) <- lift (runWriterT (instantiate script))
      let tree =
            DerivationTree
            { dtInputs = inputs
            , dtScript =
                case normalize script' of
                  TextLit builder -> LazyBuilder.toLazyText builder
            , dtName =
                case normalize name of
                  TextLit builder -> LazyBuilder.toLazyText builder
            }
      liftIO $ putStrLn "dtScript:"
      liftIO (print (dtScript tree))
      tell [tree]
      return
        (TextLit
           (fromString . show . Nix.path $
            (Nix.outputs (derivationTreeDerivation tree) Map.! "out")))
    TextLit t -> return (TextLit t)
    TextAppend l r -> do
      l <- instantiate l
      r <- instantiate r
      return (TextAppend l r)

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
      "output:out:" <> hashText ("fixed:out:" <> textHash <> ":") <> ":" <>
      nixStorePath <>
      ":" <>
      derivationName
    humanName = derivationName
    makeOutputHash str =
      let hashPart =
            str & hashlazy @SHA256 & compressHash . BS.pack . Mem.unpack &
            base32
      in T.unpack nixStorePath <> "/" <> hashPart <> "-" <> T.unpack humanName
    nixStorePath = "/nix/store"
