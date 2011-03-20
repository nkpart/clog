module Util where

import Data.List
import Data.Function

groupWithKey :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupWithKey f xs = map (\(h:t) -> (f h, h:t)) $ groupBy (on (==) f) xs
