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
import Text.Pandoc
import qualified Settings
import Debug.Trace
import Numeric (showHex, readHex)

data Post = Post { postText :: T.Text, postTime :: UTCTime, postId :: Mo.ObjectId } deriving (Eq, Show)

webId :: ObjectId -> String
webId (Oid a b) = showHex a . showChar '.' . showHex b $ ""

unWebId :: String -> ObjectId
unWebId s = let a = fst . head . readHex $ takeWhile (/= '.') s
                b = fst . head . readHex $ drop 1 . dropWhile (/= '.') $ s
             in Oid a b

-- Woo composition! 
-- I don't know why, but the markdown parser did not line carriage returns, so we filter them out.
postContent = preEscapedString . writeHtmlString defaultWriterOptions . readMarkdown defaultParserState . filter (/= '\r') . T.unpack .  postText

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

-- DELETE
deleteObject :: DbAccess m => ObjectId -> m ()
deleteObject oid = deleteOne $ Mo.select ["_id" =: oid] "posts"

-- CREATE
insertPost :: DbAccess m => String -> m Value
insertPost text = liftIO getCurrentTime >>= \t -> Mo.insert "posts" ["text" =: text, "created_at" =: t] 

-- QUERIES
lookupPost :: DbAccess m => ObjectId -> m Post
lookupPost oid = head <$> (postsBy $ withSelector ["_id" =: oid])

parsePost :: Monad m => Mo.Document -> m Post
parsePost document = do
  let text = T.pack `liftM` Mo.lookup "text" document
  let time = Mo.lookup "created_at" document
  let oid = Mo.lookup "_id" document
  Post `liftM` text `ap` time `ap` oid

postsBy :: DbAccess m => (Query -> Query) -> m [Post]
postsBy query = selectFrom "posts" query parsePost

pagePosts :: Functor m => MonadIO m => ReaderT Database (Action m) [Post]
pagePosts = postsBy (paginateQuery 10 1 . sortNatural DESC)

postsBetween :: DbAccess m => (UTCTime, UTCTime) -> m [Post]
postsBetween (start, end) = postsBy $ sortByField "created_at" DESC . withSelector ["created_at" =: ["$gt" =: start, "$lt" =: end]]

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
  let numPerPage = 10
  pg <- maybe 1 read <$> lookupGetParam "page"
  return $ paginateQuery numPerPage pg

