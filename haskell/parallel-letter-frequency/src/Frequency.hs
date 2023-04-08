module Frequency (frequency) where

import Data.Map  (Map)
import Data.Text (Text)
import Control.Parallel.Strategies

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = error "You need to implement this function."
