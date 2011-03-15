{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, PackageImports, RankNTypes #-}
module Model where

import qualified Data.Text as T
import Data.Time.Clock
import Database.MongoDB as Mo
import Yesod
import Control.Monad.Instances
import Control.Applicative
import "mtl" Control.Monad.Reader
import Data.Maybe

data Post = Post { postText :: T.Text, postTime :: UTCTime } deriving (Eq, Show)

runClog :: ReaderT Database m a -> m a
runClog = use (Database "clog")

parsePost :: Monad m => Mo.Document -> m Post
parsePost document = do
  text <- T.pack `liftM` Mo.lookup "text" document
  time <- timestamp `liftM` Mo.lookup "_id" document
  return $ Post text time

findAll :: DbAccess m => UString -> m [Document]
findAll = (rest =<<) . Mo.find . Mo.select []

posts :: Functor m => MonadIO m => Action m [Post]
posts = runClog $ findAll "posts" >>= (return . (>>= parsePost))

insertPost text = runClog $ Mo.insert "posts" $ ["text" =: text] 

