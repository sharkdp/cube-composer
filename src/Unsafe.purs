module Unsafe where

-- | Layman error handling
foreign import unsafeError :: forall a. String -> a
