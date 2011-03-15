{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Clog
import Model
import Database.MongoDB
import Data.Time.Format
import System.Locale

formattedPostTime :: Post -> String
formattedPostTime = formatTime defaultTimeLocale "%A" . postTime

getRootR :: Handler RepHtml
getRootR = do
  posts <- runDB posts
  defaultLayout $ do
    setTitle "clog homepage"
    addWidget $(widgetFile "homepage")

getNewPostR :: Handler RepHtml
getNewPostR = do
  defaultLayout $ do
    setTitle "new clog"
    addWidget $(widgetFile "posts/new")

postNewPostR :: Handler RepHtml
postNewPostR = do
  Just postText <- lookupPostParam "post_text"
  runDB $ insertPost postText 
  redirect RedirectTemporary RootR

