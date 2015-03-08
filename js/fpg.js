/*jslint nomen: true, white: true*/
/*global R*/

// This file includes only pure, functional code
// See io.js for impure (I/O) code.

'use strict';

var BL = 0;
var BR = 1;
var RE = 2;
var OR = 3;
var YE = 4;

// Basic transformations:

// Simple replacement
var replace = R.curry(function(a, b, x) {
    return (x === a) ? b : x;
});

// Repeat a list twice
// [1, 2] -> [1, 2, 1, 2]
var clone = R.partialRight(R.repeat, 2);

// Cycle colors
var cycle = R.pipe(R.add(1), R.mathMod(R.__, 5));

// Higher order functions:

// Lifts a higher order function like map or filter to the domain of
// two-dimensional arrays.
// lift2D(map) :: [[a]] -> (a -> b) -> [[b]]
var lift2D = function(f) {
    return R.compose(R.map, f);
};

var map2D = lift2D(R.map);
var filter2D = lift2D(R.filter);
var reject2D = lift2D(R.reject);

// Wrap values in lists: [1, 2, 3] -> [[1], [2], [3]]
var wrap = R.map(R.of);

var flatten2D = R.compose(wrap, R.flatten);

// var flatmap2D = R.curry(function(f, xs) {
//     return flatten2D(map2D(f)(xs));
// });

// Clears out empty lists in the 2D array:
// [[1], [], [2, 3]] -> [[1], [2, 3]]
var clearNil = R.filter(R.not(R.eqDeep([])));

// Flatten to two-dimensional array and clean out empty rows
// [[1], [], [2, [3, 4]]] -> [[1], [2, 3, 4]]
var cleanup = R.compose(clearNil, R.map(R.flatten));

// Consecutively apply the functions 'fs', returning each step
// in the transformation chain. After each function application,
// 'cleanup' is called.
var transformationSteps = function(initial, fs) {
    return R.scan(function(ls, func) {
        return cleanup(func(ls));
    }, initial, fs);
};


// Testing:
var fs = [
    map2D(clone),
    flatten2D,
    // flatmap2D(clone),
    map2D(replace(BR, [YE, BR])),
    map2D(replace(BR, [YE, BR])),
    reject2D(R.eq(BR)),
];

var init = wrap([BR, YE, BR]);

var steps = transformationSteps(init, fs);
