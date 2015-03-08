/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global R, Isomer, steps*/

'use strict';

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

var render = function(steps) {
    // render in reverse order to avoid overlapping paths
    R.forEachIndexed(renderLine, steps.reverse());
};

window.onload = function() {
    var canvas = document.getElementById('canvas');
    isomer = new Isomer(canvas);
    isomer.scale = 40;

    render(steps);
};
