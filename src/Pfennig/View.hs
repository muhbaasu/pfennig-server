{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module View where

import           Lucid

index :: Html ()
index = head_ $ do
  head_ hd
  body_ login

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
         href_ "assets/normalize.css"]
  link_ [rel_ "stylesheet",
         type_ "text/css",
         href_ "assets/generated.css"]

login :: Html ()
login =
  div_ [class_ "center"] $ do
    form_ [class_ "login"] $ do
      div_ [class_ "row"] $ do
        label_ [for_ "user"] "Username"
        input_ [id_ "user", type_ "text"]

      div_ [class_ "row"] $ do
        label_ [for_ "pass"] "Password"
        input_ [id_ "pass", type_ "password"]

      div_ [class_ "row"] $ do
        div_ $ do
          label_ [for_ "remember"] "Remember me"
          input_ [id_ "remember", type_ "checkbox"]
        div_ $ do
          a_ [href_ "#"] "Forgot password?"

      div_ [class_ "row"] $ do
        button_ [type_ "submit"] "Login"
