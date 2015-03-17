'use strict';

var R = require("ramda");
var C = require("./cube-composer");

// var BL = 0;
var BR = 1;
// var RE = 2;
var OR = 3;
var YE = 4;


module.exports = {
    'stackEqual': C.stackEqual,
    'flatten': C.flattenDeep,
    // 'map({X} ↦ {X}{X})': C.map2D(C.clone),
    // 'map({OR} ↦ {YE})': C.map2D(C.replace(OR, YE)),
    'map({YE} ↦ {BR})': C.map2D(C.replace(YE, BR)),
    'map({YE} ↦ {BR}{YE})': C.map2D(C.replace(YE, [BR, YE])),
    'map({BR} ↦ {BR}{BR}{BR})': C.map2D(C.replace(BR, [BR, BR, BR])),
    'map({BR} ↦ {OR}{OR})': C.map2D(C.replace(BR, [OR, OR])),
    'reject({OR})': C.reject2D(R.eq(OR)),
    'reject({YE})': C.reject2D(R.eq(YE)),
    // 'filter({BR})': C.filter2D(R.eq(BR)),
    'map(push({YE}))': R.map(R.append(YE)),
    'take lowest': C.lowest, // TODO
    'take tail': C.tail, // TODO
    // 'head': C.head, // TODO
    'towersOnly': C.towersOnly, // TODO
    // 'sort': R.sortBy(R.identity)
    // 'filter(contains({BR}))': R.filter(R.contains(BR))
};
