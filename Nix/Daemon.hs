{-# language PatternSynonyms #-}

module Nix.Daemon ( NixDaemon, withDaemon, addTextToStore ) where

import Control.Exception ( bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Int ( Int64 )
import Data.Text.Lazy ( Text )

import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Network.Socket as N hiding ( recv )
import qualified Network.Socket.ByteString.Lazy as N


newtype NixDaemon = NixDaemon N.Socket


pattern WOP_ADD_TEXT_TO_STORE :: ( Num a, Eq a ) => a
pattern WOP_ADD_TEXT_TO_STORE =
  8


socketPath :: FilePath
socketPath =
  "/nix/var/nix/daemon-socket/socket"


withDaemon :: ( NixDaemon -> IO a ) -> IO a
withDaemon m =
  bracket connect disconnect (m . NixDaemon)

  where

    disconnect =
      N.close

    connect = do
      s <-
        N.socket N.AF_UNIX N.Stream 0

      N.connect s ( N.SockAddrUnix socketPath )

      writeInt s 1852405859

      _WORKER_MAGIC_2 <-
        N.recv s 8

      _PROTOCOL_VERSION <-
        N.recv s 8

      writeInt s 0 -- Padding?

      return s


addTextToStore
  :: ( Foldable f, MonadIO m ) => NixDaemon -> Text -> Text -> f Text -> m Text
addTextToStore ( NixDaemon s ) name text references = do
  writeInt s WOP_ADD_TEXT_TO_STORE

  writeText s name

  writeText s text

  writeTexts s references

  _ <-
    readNum s

  _ <-
    readNum s

  readText s


readNum :: MonadIO m => N.Socket -> m Int64
readNum s = do
  Binary.runGet Binary.getInt64le <$> liftIO ( N.recv s 8 )


readText :: MonadIO m => N.Socket -> m Text
readText s = liftIO $ do
  len <-
    readNum s

  t <-
    L.decodeUtf8 <$> N.recv s ( fromIntegral len )

  case len `mod` 8 of
    n | n > 0 ->
      N.recv s ( 8 - n ) >> return ()

    _ ->
      return ()

  return t


writeInt :: MonadIO m => N.Socket -> Int64 -> m ()
writeInt s n =
  liftIO ( N.sendAll s ( BS.toLazyByteString ( BS.int64LE n ) ) )


writeText :: MonadIO m => N.Socket -> Text -> m ()
writeText s text = liftIO  $ do
  N.sendAll s ( BS.toLazyByteString ( BS.int64LE ( L.length text ) ) )

  N.sendAll s ( L.encodeUtf8 text )

  case L.length text `mod` 8 of
    n | n > 0 ->
      N.sendAll s ( BS.replicate ( 8 - n ) 0 )

    _ ->
      return ()


writeTexts :: ( Foldable f, MonadIO m ) => N.Socket -> f Text -> m ()
writeTexts s texts = do
  writeInt s ( fromIntegral ( length texts ) )

  mapM_ ( writeText s ) texts
