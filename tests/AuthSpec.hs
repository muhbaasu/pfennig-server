{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (main, spec) where

import           Auth
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)

import           Data.Maybe         (fromJust)
import           Test.Hspec
import           Web.JWT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JWToken" $ do
    it "should fail verification if nbf is before now" $ do
      let notBefore = intDate 946684800 -- 2000-01-01T00:00:00+00:00
          expires = intDate 1262304000  -- 2010-01-01T00:00:00+00:00
          cs = def { sub = stringOrURI "Marcel"
                   , Web.JWT.exp = expires
                   , nbf = notBefore }
          jwt = encodeSigned HS256 key cs
          mJwt = decodeAndVerifySignature key jwt
          now  = UTCTime (fromGregorian 1990 1 1) (secondsToDiffTime 0)
          valid = isCurrentlyValid (fromJust mJwt) now
      valid `shouldBe` False


    it "should fail verification if token is expired" $ do
      let notBefore = intDate 1136073600 -- 2006-01-01T00:00:00+00:00
          expires = intDate 1262304000   -- 2010-01-01T00:00:00+00:00
          cs = def { sub = stringOrURI "Marcel"
                   , Web.JWT.exp = expires
                   , nbf = notBefore }
          jwt = encodeSigned HS256 key cs
          mJwt = decodeAndVerifySignature key jwt
          now  = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
          valid = isCurrentlyValid (fromJust mJwt) now
      valid `shouldBe` False

    it "should fail verification if exp and nbf are missing" $ do
      let cs = def { sub = stringOrURI "Marcel"}
          jwt = encodeSigned HS256 key cs
          mJwt = decodeAndVerifySignature key jwt
          now  = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)
          valid = isCurrentlyValid (fromJust mJwt) now
      valid `shouldBe` False

    it "should succeed verification if token is not expired and is nbf" $ do
      let notBefore = intDate 946684800 -- 2000-01-01T00:00:00+00:00
          expires = intDate 1262304000  -- 2010-01-01T00:00:00+00:00
          cs = def { sub = stringOrURI "Marcel"
                   , Web.JWT.exp = expires
                   , nbf = notBefore }
          jwt = encodeSigned HS256 key cs
          mJwt = decodeAndVerifySignature key jwt
          now  = UTCTime (fromGregorian 2005 1 1) (secondsToDiffTime 0)
          valid = isCurrentlyValid (fromJust mJwt) now
      valid `shouldBe` True
