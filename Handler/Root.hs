{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Clog
import Model
import Model.MongoDB
import Database.MongoDB
import Data.Time
import System.Locale
import System.Time.Parse
import Control.Applicative ((<$>))

import qualified Settings

postStamp = showTime "%l:%M %P" . postLocalTime Settings.timeZone 

makeTime :: Day -> TimeOfDay -> UTCTime
makeTime d t = localTimeToUTC Settings.timeZone $ LocalTime d t

dayStart = TimeOfDay 0 0 0
dayEnd = TimeOfDay 23 59 59

dayToRange :: Day -> (UTCTime, UTCTime)
dayToRange d = let f = makeTime d in (f dayStart, f dayEnd)

monthToRange :: Day -> (UTCTime, UTCTime)
monthToRange start = (makeTime start dayStart, makeTime end dayEnd) 
                     where (y,m,d) = toGregorian start
                           end = fromGregorian y m (d+31)

getRootR :: Handler RepHtml
getRootR = do 
  p <- paginate
  v <- runMo $ postsBy $ p . sortNatural DESC
  renderPosts v

getPostsMonthR :: Integer -> Int -> Handler RepHtml
getPostsMonthR y m = renderPosts =<< runMo (postsBetween $ monthToRange $ fromGregorian y m 1)

getPostsDayR :: Integer -> Int -> Int -> Handler RepHtml
getPostsDayR y m d = renderPosts =<< runMo (postsBetween $ dayToRange $ fromGregorian y m d)

renderPosts posts = do
  let groupedPosts = groupPostsByDays Settings.timeZone posts
  defaultLayout $ do
    setTitle "clog"
    addWidget $(widgetFile "homepage")

getNewPostR :: Handler RepHtml
getNewPostR = do
  defaultLayout $ do
    setTitle "new clog"
    addWidget $(widgetFile "posts/new")

postNewPostR :: Handler RepHtml
postNewPostR = do
  Just postText <- lookupPostParam "post_text"
  runMo $ insertPost postText 
  redirect RedirectTemporary RootR

getCalendarR :: Handler RepHtml
getCalendarR = do
  calendarMonths <- runMo $ allPostDaysByMonth
  defaultLayout $ do
    setTitle "clog calendar"
    addWidget $(widgetFile "calendar")

