{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Clog

import Database.MongoDB

addGoogleWebFont = addStylesheetRemote . ("http://fonts.googleapis.com/css?family="++)

getRootR :: Handler RepHtml
getRootR = do
    let u = ("lol"::String)
    let title =  ("Clog" :: String)

    title <- runDB $ use (Database "clog") $ do
      return ("Clog" :: String)

    defaultLayout $ do
        h2id <- lift $ return ("ff" :: String)
        mapM_ addGoogleWebFont ["Bevan", "Inconsolata"]
        setTitle "clog homepage"
        addWidget $(widgetFile "homepage")
