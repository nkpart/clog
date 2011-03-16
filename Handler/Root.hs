{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Clog
import Model
import Database.MongoDB
import Data.Time.Format
import System.Locale
import Data.Time.LocalTime
import System.Time.Parse

import qualified Settings

postStamp = showTime "%l:%M %P" . postLocalTime Settings.timeZone 

parseDay = parseCalendarTime defaultTimeLocale "%Y%m%d"

getRootR :: Handler RepHtml
getRootR = do
  mdayRaw <- lookupGetParam "day"
  let mday = mdayRaw >>= parseDay
  posts <- runDB $ maybe posts postsOn mday
  let groupedPosts = postDays Settings.timeZone posts
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

