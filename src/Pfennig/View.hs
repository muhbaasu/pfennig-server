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
         href_ "https://storage.googleapis.com/code.getmdl.io/1.0.3/material.indigo-pink.min.css"]
  script_ [src_ "https://storage.googleapis.com/code.getmdl.io/1.0.3/material.min.js"] ""
  link_ [rel_ "stylesheet",
         href_ "https://fonts.googleapis.com/icon?family=Material+Icons"]
  link_ [rel_ "stylesheet",
         type_ "text/css",
         href_ "assets/generated.css"]

main' :: Html ()
main' = do
  div_ [class_ "mdl-layout mdl-js-layout mdl-layout--fixed-header"] $ do
    header_ [class_ "mdl-layout__header"] $
      div_ [class_ "mdl-layout__header-row"] $ do
        div_ [class_ "title"] "pfennig"
        div_ [class_ "mdl-layout-spacer"] ""
        nav_ [class_ "mdl-navigation mdl-layout--large-screen-only"] $ do
          a_ [href_ "/logout"] "Logout"
    main_ [class_ "mdl-layout__content"] $
      h1_ "Welcome to the Aperture Enrichment Center!"

login :: Html ()
login =
  div_ [class_ "center"] $
    form_ [class_ "login", method_ "post", action_ "/login"] $ do
      h3_ "Login"

      div_ [class_ "row mdl-textfield mdl-js-textfield mdl-textfield--floating-label"] $ do
        input_ [id_ "email",
                class_ "mdl-textfield__input",
                name_ "email",
                type_ "email"]
        label_ [class_ "mdl-textfield__label",
                for_ "email"] "E-Mail"

      div_ [class_ "row mdl-textfield mdl-js-textfield mdl-textfield--floating-label"] $ do
        input_ [id_ "pass",
                class_ "mdl-textfield__input",
                name_ "pass",
                type_ "password"]
        label_ [class_ "mdl-textfield__label",
                for_ "pass"] "Password"

      div_ [class_ "row mdl-textfield"] $ do
        label_ [for_ "remember",
                class_ "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect"] $ do
          input_ [id_ "remember",
                  type_ "checkbox",
                  name_ "remember",
                  value_ "remember",
                  class_ "mdl-checkbox__input"]
          span_ [class_ "mdl-checkbox__label"] "Remember me"

      div_ [class_ "row"] $ do
        a_ [href_ "/register",
            class_ "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"] "Register"
        a_ [href_ "#",
            class_ "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect"] "Forgot password?"

      div_ [class_ "row"] $
        button_ [type_ "submit",
                 class_ "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored",
                 value_ "submit"] "Login"

register :: Html ()
register =
  div_ [class_ "center"] $
    form_ [class_ "registration column", method_ "post", action_ "/register"] $ do
      h3_ "Register"

      div_ [class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"] $ do
        input_ [id_ "email",
                class_ "mdl-textfield__input",
                name_ "email",
                type_ "email",
                required_ "",
                autofocus_]
        label_ [class_ "mdl-textfield__label",
                for_ "email"] "E-Mail"

      div_ [class_ "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"] $ do
        input_ [id_ "pass",
                class_ "mdl-textfield__input",
                name_ "pass",
                pattern_ ".{5,128}",
                required_ "",
                type_ "password"]
        label_ [class_ "mdl-textfield__label",
                for_ "pass"] "Password"
        span_ [class_"mdl-textfield__error"] "Password must contain at least five characters"

      div_ [class_ "row"] $
        button_ [type_ "submit",
                 class_ "frm-btn mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--colored",
                 value_ "submit"] "Register"

notFound :: Html ()
notFound = div_ [] "No matching route found"
