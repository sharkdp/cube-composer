/* global exports */
"use strict";

// module Levels.Chapter3

exports.digits = function(num) {
    var bin = ((num % 8 + 8) % 8).toString(2);
    bin = "000".split("").slice(0, 3 - bin.length).join("") + bin;
    return bin.split("").map(Number).reverse();
};
