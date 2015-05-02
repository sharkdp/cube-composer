module Helper where

import Data.Tuple

infixl 6 :>

-- | Operator to create tuples, especially for creating maps with
-- | `Map.fromList ["key1" :> "value1", "key2" :> "value2"]`
(:>) :: forall a b. a -> b -> Tuple a b
(:>) a b = Tuple a b
