module View where

import           Lucid

index :: Html ()
index = head_ $ do
  head_ $ title_ "Enrichment center"
  body_ $ span_ "Welcome to the test chamber."
