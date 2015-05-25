module Unsafe where

-- | Layman error handling
foreign import unsafeError
    """
    function unsafeError(msg) {
        throw new Error(msg);
    }
    """ :: forall a. String -> a
