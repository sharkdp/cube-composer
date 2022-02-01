module ListHelper where

import Prelude
import Data.List (List, elemIndex)
import Data.Maybe (isJust)

contains :: forall a. (Eq a) => a -> List a -> Boolean
contains x xs = isJust $ elemIndex x xs
