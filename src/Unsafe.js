exports.unsafeError = function(msg) {
    // Try to recover from this error by resetting the global state.
    if (confirm(msg + ". Clear localStorage and reload?")) {
        localStorage.clear();
        location.reload();
    }

    // Abort execution
    throw new Error(msg);
};
