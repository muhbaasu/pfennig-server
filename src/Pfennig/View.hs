{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module View where

import           Lucid

index :: Html () -> Html ()
index bd = head_ $ do
  head_ hd
  body_ bd

hd :: Html ()
hd = do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
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
    form_ [class_ "login", method_ "post", action_ "/login"] $ do
      h3_ "Login"

      div_ [class_ "row"] $ do
        input_ [id_ "email", name_ "email", placeholder_ "E-Mail", type_ "text"]

      div_ [class_ "row"] $ do
        input_ [id_ "pass", name_ "pass", placeholder_ "Password", type_ "password"]

      div_ [class_ "row"] $ do
        label_ [for_ "remember"] "Remember me"
        input_ [id_ "remember", type_ "checkbox"]

      div_ [class_ "row"] $ do
        a_ [href_ "/register"] "Register"
        a_ [href_ "#"] "Forgot password?"

      div_ [class_ "row"] $ do
        button_ [type_ "submit"] "Login"

register :: Html ()
register =
  div_ [class_ "center"] $ do
    form_ [class_ "registration", method_ "post", action_ "/registration"] $ do
      div_ [class_ "row"] $ do
        label_ [for_ "email"] "E-Mail"
        input_ [id_ "email", name_ "email", type_ "text"]
      div_ [class_ "row"] $ do
        label_ [for_ "pass"] "Password"
        input_ [id_ "pass", name_ "pass", type_ "password"]
      div_ [class_ "row"] $ do
        button_ [type_ "submit"] "Register"
