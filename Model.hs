{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, PackageImports, RankNTypes #-}
module Model where

import qualified Data.Text as T
import System.Locale
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.CalendarTime
import Database.MongoDB as Mo
import Yesod
import Control.Monad.Instances
import Control.Applicative
import "mtl" Control.Monad.Reader
import Data.Maybe
import Data.List

data Post = Post { postText :: T.Text, postTime :: UTCTime } deriving (Eq, Show)

postLocalTime zone = utcToLocalTime zone . postTime

postLocalDay zone = localDay . postLocalTime zone

showTime :: FormatTime t => String -> t -> String
showTime = formatTime defaultTimeLocale

runClog :: ReaderT Database m a -> m a
runClog = use (Database "clog")

parsePost :: Monad m => Mo.Document -> m Post
parsePost document = do
  text <- T.pack `liftM` Mo.lookup "text" document
  time <- timestamp `liftM` Mo.lookup "_id" document
  return $ Post text time

findBy :: DbAccess m => (Query -> Query) -> UString -> m [Document]
findBy p s = (rest =<<) . Mo.find . p $ Mo.select [] s

findAll :: DbAccess m => UString -> m [Document]
findAll = (rest =<<) . Mo.find . (\a -> a{Mo.sort = ["$natural" =: (-1::Int)]}) . Mo.select []

posts :: Functor m => MonadIO m => Action m [Post]
posts = runClog $ findBy (\a -> a{Mo.sort = ["$natural" =: (-1::Int)]}) "posts" >>= (return . (>>= parsePost))

postsOn :: Functor m => MonadIO m => CalendarTime -> Action m [Post]
postsOn time = runClog $ findBy (\a -> a{Mo.sort = ["$natural" =: (-1::Int)]}) "posts" >>= (return . (>>= parsePost))

eqOn f a b = f a == f b

postDays :: TimeZone -> [Post] -> [(String, [Post])]
postDays zone posts = map (\xs -> (showTime "%A, %e %B" . postLocalDay zone $ head xs, xs)) $ groupBy (eqOn $ postLocalDay zone) posts

insertPost text = runClog $ Mo.insert "posts" $ ["text" =: text] 

