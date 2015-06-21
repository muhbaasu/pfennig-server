{-# LANGUAGE OverloadedStrings #-}

module Layout where

import           Clay              hiding ((**))
import           Data.Monoid
import qualified Data.Text.Lazy.IO as LIO
import           Prelude           hiding (rem)

renderCSS :: IO ()
renderCSS = do
  let css = render pfennig
  LIO.writeFile "./assets/generated.css" css

pfennig :: Css
pfennig = do
  body ? do
    fstFontFamily

fstFontFamily = fontFamily ["Roboto"] [sansSerif]

--------------------------------------------------------------------------------
--- http://csswizardry.com/2012/02/pragmatic-practical-font-sizing-in-css/

fonts :: Css
fonts = do
  giga
  mega
  kilo
  alpha
  beta
  gamma
  delta
  epsilon
  milli

giga :: Css
giga = ".giga" ? do
  fontSize (px 80)
  fontSize (rem 5)
  lineHeight (em 1.2)

mega :: Css
mega = ".mega" ? do
  fontSize (px 64)
  fontSize (rem 4)
  lineHeight (em 1.125)

kilo :: Css
kilo = ".kilo" ? do
  fontSize (px 48)
  fontSize (rem 3)
  lineHeight (em 1)

alpha :: Css
alpha = h1 <> ".alpha" ? do
  fontSize (px 32)
  fontSize (rem 2)
  lineHeight (em 1.5)

beta :: Css
beta = h2 <> ".beta" ? do
  fontSize (px 24)
  fontSize (rem 1.5)
  lineHeight (em 1)

gamma :: Css
gamma = h3 <> ".gamma" ? do
  fontSize (px 20)
  fontSize (rem 1.25)
  lineHeight (em 1.2)

delta :: Css
delta = h4 <> ".delta" ? do
  fontSize (px 18)
  fontSize (rem 1.125)
  lineHeight (em 1.333)

epsilon :: Css
epsilon = h5 <> ".epsilon" <> h6 <> ".zeta" ? do
  fontSize (px 16)
  fontSize (rem 1)
  lineHeight (em 1.5)

milli :: Css
milli = ".milli" ? do
  fontSize (px 12)
  fontSize (rem 0.75)
  lineHeight (em 2)
