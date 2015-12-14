{-# LANGUAGE DeriveGeneric #-}

module Events.Recorder where

import GHC.Generics (Generic)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import qualified Data.Binary as B
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as BL
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as TBQ

-- | Prepend a bytestring with a 8-byte wide size tag
toSizeTaggedByteString :: BL.ByteString -> BL.ByteString
toSizeTaggedByteString = BP.runPut . putSizeTaggedByteString

-- | Read a size-tagged bytestring and the unconsumed rest
fromSizeTaggedByteString :: BL.ByteString
                         -> Maybe (BL.ByteString, BL.ByteString)
fromSizeTaggedByteString =
  resultToMaybe . BG.runGetOrFail getSizeTaggedByteString
  where resultToMaybe (Left _) = Nothing
        resultToMaybe (Right (restBs, _, resultBs)) = Just (resultBs, restBs)

-- | Reader for a size-tagged bytestring
getSizeTaggedByteString :: BG.Get BL.ByteString
getSizeTaggedByteString = do
  size <- fromIntegral <$> BG.getWord64le
  BG.getLazyByteString size

-- | Writer for a size-tagged bytestring
putSizeTaggedByteString :: BL.ByteString -> BP.Put
putSizeTaggedByteString bs = do
  let size = fromIntegral $ BL.length bs
  BP.putWord64le size
  BP.putLazyByteString bs

recordEvents :: B.Binary a => TBQ.TBQueue a -> IO ThreadId
recordEvents q = forkIO go
  where go = do
          evt <- STM.atomically $ TBQ.readTBQueue q
          let bs = B.encode evt
          tid <- myThreadId
          putStrLn $ "[Thread " ++ show tid ++ "] Got a message:"
          putStrLn $ show bs
          go

data Event = Hello String
           | Goodbye String
           deriving (Show, Generic)

instance B.Binary Event
