{-# LANGUAGE OverloadedStrings #-}

module Layout where

import           Clay                    hiding ((**))
import           Clay.Stylesheet         (prefixed)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Monoid
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
    -- backgroundColor backgroundColor'
    -- color foregroundColor'

  star ?
    boxSizing inherit

  base'

  ".title" ? do
    fontFamily ["comfortaa"] [sansSerif]
    fontFace $ importUrl "assets/comfortaa-regular-webfont.woff"
    fontSize (rem 1.4)


  -- forms

  -- body ?
  --   fstFontFamily

  -- (h1 <> h2 <> h3) ?
  --   color accentColor'

-- blue' :: Color
-- blue' = rgb 33 150 243

-- purple' :: Color
-- purple' = rgb 156 39 176

-- backgroundColor' :: Color
-- backgroundColor' = rgb 249 249 249

-- foregroundColor' :: Color
-- foregroundColor' = rgb 33 33 33

-- lightGrey' :: Color
-- lightGrey' = rgb 238 238 238

-- accentColor' :: Color
-- accentColor' = rgb 0 150 136

-- shadowColor' :: Color
-- shadowColor' = rgba 0 0 0 64

-- button' :: Css
-- button' =
--   button ? do
--     backgroundColor blue'
--     border none nil transparent
--     color white
--     padding (vh 0.75) (vw 2) (vh 0.75) (vw 2)
--     textTransform uppercase

base' :: Css
base' = do
  ".row" ? do
    display flex
    alignItems center
    flexDirection row

  ".column" ? do
    display flex
    flexDirection column

  ".center" ? do
    display flex
    height $ pct 100
    width $ pct 100
    justifyContent center
    alignItems center

  ".frm-btn"? do
    flexGrow 1
    margin (vmin 0.6) (vmin 0.6) (vmin 0.6) (vmin 0.6)

--   button'
--   input'
--   login'

-- input' :: Css
-- input' =
--   (input # ("type" ^= "password")) <> (input # ("type" ^= "text")) <? do
--     focus & do
--       outline none nil transparent
--       borderBottom double (px 1) accentColor'

--     border none nil transparent
--     borderBottom solid (px 1) lightGrey'
--     width $ pct 100

-- forms :: Css
-- forms = do
--   form ? do
--     background white
--     border solid (px 1) lightGrey'
--     paddingAll (vh 2)

--   form |> ".row" ? do
--     justifyContent spaceBetween
--     margin nil nil (vh 1.5) nil

-- login' :: Css
-- login' =
--   ".login" ? do
--     boxShadow' nil (vh 3) (vh 5) (vh (-1.5)) shadowColor'
--     width (pct 30)

-- boxShadow' :: Size a -> Size a -> Size a -> Size a -> Color -> Css
-- boxShadow' x y w z c = prefixed (browsers <> "box-shadow") (x ! y ! w ! z ! c)

-- fstFontFamily :: Css
-- fstFontFamily = fontFamily ["Roboto"] [sansSerif]

-- paddingAll :: Size a -> Css
-- paddingAll s' = padding s' s' s' s'

-- --------------------------------------------------------------------------------
-- --- http://csswizardry.com/2012/02/pragmatic-practical-font-sizing-in-css/

-- fonts :: Css
-- fonts = do
--   giga
--   mega
--   kilo
--   alpha
--   beta
--   gamma
--   delta
--   epsilon
--   milli

-- giga :: Css
-- giga = ".giga" ? do
--   fontSize (px 80)
--   fontSize (rem 5)
--   lineHeight (em 1.2)

-- mega :: Css
-- mega = ".mega" ? do
--   fontSize (px 64)
--   fontSize (rem 4)
--   lineHeight (em 1.125)

-- kilo :: Css
-- kilo = ".kilo" ? do
--   fontSize (px 48)
--   fontSize (rem 3)
--   lineHeight (em 1)

-- alpha :: Css
-- alpha = h1 <> ".alpha" ? do
--   fontSize (px 32)
--   fontSize (rem 2)
--   lineHeight (em 1.5)

-- beta :: Css
-- beta = h2 <> ".beta" ? do
--   fontSize (px 24)
--   fontSize (rem 1.5)
--   lineHeight (em 1)

-- gamma :: Css
-- gamma = h3 <> ".gamma" ? do
--   fontSize (px 20)
--   fontSize (rem 1.25)
--   lineHeight (em 1.2)

-- delta :: Css
-- delta = h4 <> ".delta" ? do
--   fontSize (px 18)
--   fontSize (rem 1.125)
--   lineHeight (em 1.333)

-- epsilon :: Css
-- epsilon = h5 <> ".epsilon" <> h6 <> ".zeta" ? do
--   fontSize (px 16)
--   fontSize (rem 1)
--   lineHeight (em 1.5)

-- milli :: Css
-- milli = ".milli" ? do
--   fontSize (px 12)
--   fontSize (rem 0.75)
--   lineHeight (em 2)
