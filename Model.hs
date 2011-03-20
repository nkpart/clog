{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings, PackageImports, RankNTypes #-}
module Model where

import Model.MongoDB
import qualified Data.Text as T
import System.Locale
import Data.Time
import Database.MongoDB as Mo
import Yesod
import Control.Monad.Instances
import Data.Function
import Control.Applicative
import "mtl" Control.Monad.Reader
import Data.Maybe
import Data.List
import Util
import qualified Settings

data Post = Post { postText :: T.Text, postTime :: UTCTime } deriving (Eq, Show)

data CalendarMonth = CalendarMonth Integer Int [Int] deriving (Eq, Show)

monthName :: CalendarMonth -> String
monthName (CalendarMonth y m _) = showTime "%B" $ fromGregorian y m 1

monthYear :: CalendarMonth -> Integer
monthYear (CalendarMonth y _ _) = y

daysIn (CalendarMonth _ _ ds) = ds

postLocalTime :: TimeZone -> Post -> LocalTime
postLocalTime zone = utcToLocalTime zone . postTime
 
showTime :: FormatTime t => String -> t -> String
showTime = formatTime defaultTimeLocale

-- CREATE
insertPost :: DbAccess m => String -> m Value
insertPost text = liftIO getCurrentTime >>= \t -> Mo.insert "posts" ["text" =: text, "created_at" =: t] 

-- QUERIES
parsePost :: Monad m => Mo.Document -> m Post
parsePost document = do
  text <- T.pack `liftM` Mo.lookup "text" document
  time <- Mo.lookup "created_at" document
  return $ Post text time

postsBy :: DbAccess m => (Query -> Query) -> m [Post]
postsBy query = selectFrom "posts" query parsePost

pagePosts :: Functor m => MonadIO m => ReaderT Database (Action m) [Post]
pagePosts = postsBy (paginateQuery 10 1 . sortNatural DESC)

postsBetween :: DbAccess m => (UTCTime, UTCTime) -> m [Post]
postsBetween (start, end) = postsBy (sortByField "created_at" DESC . (\a -> a { 
                                    Mo.selection = (Mo.selection a) { Mo.selector = ["created_at" =: ["$gt" =: start, "$lt" =: end]] }
                                  }))

groupPostsByDays :: TimeZone -> [Post] -> [(String, [Post])]
groupPostsByDays zone posts = let byDay = groupWithKey (localDay . postLocalTime zone) posts in
                              map (\(day, ps) -> (showTime "%A, %e %B" day, ps)) byDay

allPostTimes :: DbAccess m => m [UTCTime]
allPostTimes = selectFrom "posts" (projectFields ["created_at"]) (Mo.lookup "created_at")

allPostDaysByMonth :: DbAccess m => m [CalendarMonth]
allPostDaysByMonth = do
  let zone = Settings.timeZone
  times <- map (toGregorian . localDay . utcToLocalTime zone) <$> allPostTimes
  let byCM = groupWithKey (\(a,b,c) -> (a,b)) times
  return $ map (\((y, m), times) -> CalendarMonth y m (nub $ map (\(_,_,d) -> d) times)) byCM

paginate :: Functor m => RequestReader m => Monad m => m (Query -> Query)
paginate = do
  let numPerPage = 2
  pg <- maybe 1 read <$> lookupGetParam "page"
  return $ paginateQuery numPerPage pg

