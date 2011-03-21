{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Model.MongoDB (
    selectFrom
  , Sort(..)
  --- Combinators
  , sortNatural
  , paginateQuery
  , projectFields
  , sortByField
  , withSelector
  ) where

import qualified GHC.Word
import Database.MongoDB as Mo
import Data.Typeable

-- Top level finder thing
selectFrom :: DbAccess m => UString -> (Query -> Query) -> (Document -> [a]) -> m [a]
selectFrom s p parse = do
  docs <- (rest =<<) . Mo.find . p $ Mo.select [] s
  return $ docs >>= parse

data Sort = ASC | DESC deriving (Eq, Show, Typeable)

instance Val Sort where
  val ASC = Int32 1
  val DESC = Int32 (-1)

  cast' (Int32 1) = Just ASC
  cast' (Int32 (-1)) = Just DESC
  cast' _ = Nothing

-- Combinators!
sortByField :: Label -> Sort -> Query -> Query
sortByField field sort q = q { Mo.sort = [field =: sort] }

sortNatural :: Sort -> Query -> Query
sortNatural = sortByField "$natural"

paginateQuery :: GHC.Word.Word32 -> GHC.Word.Word32 -> Query -> Query
paginateQuery numPerPage pageNum q = q { Mo.skip = (pageNum - 1) * numPerPage, Mo.limit = numPerPage }

projectFields :: [Label] -> Query -> Query
projectFields xs qry = qry { project = map (=: (1::Int)) xs }

withSelector :: Selector -> Query -> Query
withSelector s q = q { Mo.selection = (Mo.selection q) { Mo.selector = s } }

