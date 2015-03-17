'use strict';

var R = require("ramda");
var C = require("./cube-composer");
var functions = require("./collection");

var solveIn = function(init, target, steps, fs, chain) {
    if (steps === 0) {
        if (R.eqDeep(init, target)) {
            return chain;
        } else {
            return false;
        }
    } else {
        var res = false;

        R.find(function(fid) {
            var f = fs[fid];
            var transformed = C.cleanup(f(init));
            res = solveIn(transformed, target, steps - 1, R.omit([fid], fs), R.append(fid, chain));
            if (res) {
                return true;
            }
            return false;
        }, R.keys(fs));

        return res;
    }
};

var maxSteps = 6;

var solve = function(init, target) {
    var steps, res;

    for (steps = 0; steps <= maxSteps; steps += 1) {
        res = solveIn(init, target, steps, functions, []);
        if (res) {
            return res;
        }
    }
    return false;
};

module.exports = solve;
