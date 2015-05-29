module Unsafe where

-- | Layman error handling
foreign import unsafeError
    """
    function unsafeError(msg) {
        // Try to recover from this error by resetting the global state.
        if (confirm(msg + ". Clear localStorage and reload?")) {
           localStorage.clear();
           location.reload();
        };

        // Abort execution
        throw new Error(msg);
    }
    """ :: forall a. String -> a
