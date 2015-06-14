{-# LANGUAGE OverloadedStrings #-}

module NetworkHelperSpec (main, spec) where

import           NetworkHelper
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "matchMediaType" $ do
    it "should return nothing when no media type matches" $ do
      let m = matchMediaType [("Accept", "text/json, text/json, text/json;")]
      m `shouldBe` Nothing

    it "should return just mediatype for known matches" $ do
      let m = matchMediaType [("Accept", "application/json, application/xml, text/html;")]
      m `shouldBe` Just applicationJSON

    it "should return the clients preferred media type" $ do
      let m = matchMediaType [("Accept", "application/json;q=0.3, application/xml;q=0.7;")]
      m `shouldBe` Just applicationXML

    it "should be nothing when accept header is empty" $ do
      let m = matchMediaType [("Accept", "")]
      m `shouldBe` Nothing
