exports.unsafeLoadGameState = function (just) {
    return function(nothing) {
        return function() {
            var data = localStorage.getItem('gameState');
            if (!data) {
                return nothing;
            }
            return just(JSON.parse(data));
        };
    };
};

exports.unsafeSaveGameState = function (gs) {
    return function() {
        localStorage.setItem('gameState', JSON.stringify(gs));
        return {};
    };
};
