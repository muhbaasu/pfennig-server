{-# LANGUAGE OverloadedStrings #-}

module NetworkHelper where

import           Data.Foldable      (find)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Media (MediaType, matchAccept, parseAccept, (//))


applicationJSON :: MediaType
applicationJSON = "application" // "json"

applicationXML :: MediaType
applicationXML = "application" // "xml"

textHTML :: MediaType
textHTML = "text" // "html"

supportedMediaTypes :: [MediaType]
supportedMediaTypes = [applicationJSON, applicationXML, textHTML]

-- | lookup accept header and check if any media type is supported
--   returns the best possible match
matchMediaType :: [(T.Text, T.Text)] -> Maybe MediaType
matchMediaType hdrs = do
  val <- snd <$> find ((=="Accept") . fst) hdrs
  q <- parseAccept $ encodeUtf8 val
  matchAccept supportedMediaTypes q
