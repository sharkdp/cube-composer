/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global R, Isomer*/

'use strict';

(function () {
    var Shape = Isomer.Shape;
    var Point = Isomer.Point;
    var Color = Isomer.Color;

    // Global isomer instance
    var isomer;

    var config = {
        // http://www.colourlovers.com/palette/1473/Ocean_Five
        colors: [
            new Color(0, 160, 176),
            new Color(106, 74, 60),
            new Color(204, 51, 63),
            new Color(235, 104, 65),
            new Color(237, 201, 81)
        ],
        ymax: 13,
        xmax: 4
    };

    var renderBlock = function(x, y, value, z) {
        isomer.add(
            new Shape.Prism(
                new Point(4.2 * (config.xmax - x), config.ymax - y, z),
                1, 1, 1
            ),
            config.colors[value]
        );
    };

    var renderBlocks = function(x, value, y) {
        R.forEachIndexed(R.partial(renderBlock, x, y), value);
    };

    var renderLine = function(line, x) {
        R.forEachIndexed(R.partial(renderBlocks, x), line);
    };

    var renderLines = function(lines) {
        R.forEachIndexed(renderLine, lines.reverse());
    };

    window.onload = function() {
        var canvas = document.getElementById("canvas");
        isomer = new Isomer(canvas);
        isomer.scale = 40;

        var BL = 0;
        var BR = 1;
        var RE = 2;
        var OR = 3;
        var YE = 4;

        var replace = R.curry(function(a, b, x) {
            return (x === a) ? b : x;
        });

        // TODO: with the new rambda > 0.10, this can be replace by: R.repeat(R.__, 2);
        var clone = R.partialRight(R.repeat, 2);

        var map2d = function(f) { return R.map(R.map(f)); };
        var filter2d = function(f) { return R.map(R.filter(f)); };
        var reject2d = function(f) { return R.map(R.reject(f)); };
        var wrap = R.map(R.of);
        var flatten = R.compose(wrap, R.flatten);
        // var flatmap = R.compose(flatten, map2d);


        var queue = [
            map2d(clone),
            flatten,
            map2d(replace(YE, [YE, BR])),
            map2d(replace(BR, [RE, BR])),
            R.reject(R.eqDeep([RE, BR]))
            // reject2d(R.eq(BR)),
        ];

        var initial = wrap([YE, BR, YE]);

        var clearEmpty = R.filter(R.not(R.eqDeep([])));
        var cleanup = R.compose(clearEmpty, R.map(R.flatten));

        var lines = R.scan(function(ls, func) {
            return cleanup(func(ls));
        }, initial, queue);

        R.forEach(function(l) {console.log(l);}, lines);
        renderLines(lines);
    };
}());
