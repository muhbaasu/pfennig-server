{-# LANGUAGE OverloadedStrings #-}

module Layout where

import           Clay                    hiding ((**))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Monoid
import           Data.Text.Internal.Lazy
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO       as LIO
import           Prelude                 hiding (rem)

renderCSS :: IO ()
renderCSS = do
  let css = render pfennig
  LIO.writeFile "./assets/generated.css" css

readCSS :: ByteString
readCSS = encodeUtf8 $ render pfennig

pfennig :: Css
pfennig = do
  html ? do
    boxSizing borderBox

  star ? do
    boxSizing inherit

  base'
  forms

  login

  body ? do
    fstFontFamily

base' :: Css
base' = do
  ".row" ? do
    display flex
    alignItems center
    flexDirection row

  ".column" ? do
    display flex
    alignItems center
    flexDirection column

  ".center" ? do
    display flex
    height $ pct 100
    width $ pct 100
    justifyContent center
    alignItems center

forms :: Css
forms = do
  form ? do
    background red
    paddingAll $ rem 0.25

  form |> ".row" ? do
    justifyContent spaceBetween
    paddingAll $ rem 0.25
  ".row" |> input ? do
    marginLeft $ rem 1

login :: Css
login = do
  ".login" ? do
    background transparent

fstFontFamily = fontFamily ["Roboto"] [sansSerif]

paddingAll :: Size a -> Css
paddingAll s' = padding s' s' s' s'

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
