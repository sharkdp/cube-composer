'use strict';

var C = require("./cube-composer");
var solve = require("./solve");

// var BL = 0;
var BR = 1;
// var RE = 2;
var OR = 3;
var YE = 4;

var init = C.wrap([BR, OR, OR, YE, YE, YE, OR, OR, BR]);
var target = [[BR, BR], [BR, BR]];

console.log(init);
console.log(target);

var res = solve(init, target);
if (res) {
    console.log("solution in " + res.length.toString() + " steps!");
    console.log(res);
} else {
    console.log("no solution found");
}
