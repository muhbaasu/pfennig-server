{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module View where

import           Lucid

index :: Html ()
index = head_ $ do
  head_ hd
  body_ bd

hd :: Html ()
hd = do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport",content_ "width=device-width, initial-scale=1"]
  title_ "Enrichment center"
  link_ [rel_ "stylesheet",
         type_ "text/css",
         href_ "http://fonts.googleapis.com/css?family=Roboto"]
  link_ [rel_ "stylesheet",
         type_ "text/css",
         href_ "assets/generated.css"]

bd :: Html ()
bd = span_ "Welcome to the test chamber."
