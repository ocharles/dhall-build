{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Nix.StorePath ( derivationOutputPath, textPath ) where

import Crypto.Hash ( SHA256, hashlazy )
import Data.Bits ( (.&.), (.|.), shiftL, shiftR, xor )
import Data.Function ( (&) )
import Data.Monoid ( (<>) )
import Data.Text.Lazy ( Text )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Int ( Int64 )

import qualified Data.ByteArray as Mem
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Nix.Derivation as Nix


-- | Given a Derivation, calculate its output path.

derivationOutputPath :: Text -> Nix.Derivation -> String
derivationOutputPath derivationName derivation =
  derivation
    & clearOutputs
    & hashDerivation
    & encodeUtf8 . formatOutputDescription
    & makeOutputHash

  where

    hashDerivation =
      T.pack
        . show
        . hashlazy @SHA256
        . encodeUtf8
        . LazyBuilder.toLazyText
        . Nix.buildDerivation

    clearOutputs drv =
      drv
        { Nix.outputs = Nix.DerivationOutput "" "" "" <$ Nix.outputs drv
        , Nix.env = Map.union ("" <$ Nix.outputs drv) (Nix.env drv)
        }

    formatOutputDescription derivationHash =
      "output:out:sha256:"
        <> derivationHash
        <> ":"
        <> nixStorePath
        <> ":"
        <> derivationName

    humanName =
      derivationName

    makeOutputHash str =
      let
        hashPart =
          str
            & hashlazy @SHA256
            & compressHash . BS.pack . Mem.unpack
            & base32
      in
        T.unpack nixStorePath <> "/" <> hashPart <> "-" <> T.unpack humanName

    nixStorePath =
      "/nix/store"


-- | Given some 'Text' and the name of the file, calculate its location in the
-- Nix store.

textPath :: Text -> Text -> String
textPath derivationName text =
  text
    & hashText
    & encodeUtf8 . formatOutputDescription
    & makeOutputHash

  where

    hashText =
      T.pack . show . hashlazy @SHA256 . encodeUtf8

    formatOutputDescription textHash =
      "text:sha256"
        <> ":"
        <> textHash
        <> ":"
        <> nixStorePath
        <> ":"
        <> derivationName

    humanName =
      derivationName

    makeOutputHash str =
      let
        hashPart =
          str
            & hashlazy @SHA256
            & compressHash . BS.pack . Mem.unpack
            & base32
      in
        T.unpack nixStorePath <> "/" <> hashPart <> "-" <> T.unpack humanName

    nixStorePath =
      "/nix/store"

bsChunks :: Int64 -> BS.ByteString -> [ BS.ByteString ]
bsChunks n bytes =
  if BS.length bytes > n then
    BS.take n bytes : bsChunks n ( BS.drop n bytes )
  else
    [ bytes ]


compressHash :: BS.ByteString -> BS.ByteString
compressHash hash =
  foldl
    ( \hash' chunk -> BS.pack (BS.zipWith xor hash' chunk) )
    ( BS.replicate 20 0 )
    ( map
        ( \c ->
            BS.append c ( BS.take ( 20 - BS.length c ) ( BS.replicate 20 0 ) )
        )
        ( bsChunks 20 hash )
    )


base32 :: BS.ByteString -> String
base32 bytes =
  map
    ( \n ->
        let
          b =
            n * 5

          i =
            b `div` 8

          j =
            b `mod` 8

          c =
            shiftR ( BS.head ( BS.drop ( fromIntegral i ) bytes ) ) j
              .|.
                ( if fromIntegral i >= BS.length bytes - 1 then
                    0
                  else
                    shiftL
                      ( BS.head
                          ( BS.drop (fromIntegral (i + 1)) bytes )
                      )
                      ( 8 - j )
                )
        in
          alphabet !! fromIntegral (c .&. 0x1f)
    )
    ( reverse [0 .. 32 - 1 :: Int] )

  where

    alphabet =
      "0123456789abcdfghijklmnpqrsvwxyz"
