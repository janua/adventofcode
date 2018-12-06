module Utils where

import Data.String (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.List (List)
import Data.Array (toUnfoldable)

newlinePattern :: Pattern
newlinePattern = Pattern "\n"

splitStringAtNewlines :: String -> List String
splitStringAtNewlines s =
    toUnfoldable (split newlinePattern s)