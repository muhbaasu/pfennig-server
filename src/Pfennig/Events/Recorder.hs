{-# LANGUAGE DeriveGeneric #-}

module Recorder where

import GHC.Generic (Generic)
import qualified Data.Binary as B
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as BL
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

getSizeTaggedByteString :: BG.Get BL.ByteString
getSizeTaggedByteString = do
  size <- fromIntegral <$> BG.getWord64le
  BG.getLazyByteString size

putSizeTaggedByteString :: BL.ByteString -> BP.Put
putSizeTaggedByteString bs = do
  let size = fromIntegral $ BL.length bs
  BP.putWord64le size
  BP.putLazyByteString bs

recordEvents :: Binary a => TBQ.TBQueue a -> IO ()
recordEvents = undefined
