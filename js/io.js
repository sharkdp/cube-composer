/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global $, R, Isomer, steps*/

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

    render([init]);

    var functions = {
        'stackEqual': stackEqual,
        'flatten': flattenDeep,
        'map(clone)': map2D(clone),
        'map(OR ↦ YE)': map2D(replace(OR, YE)),
        'map(YE ↦ BR)': map2D(replace(YE, BR)),
        'map(YE ↦ [BR, YE])': map2D(replace(YE, [BR, YE])),
        'map(BR ↦ [BR, BR, BR])': map2D(replace(BR, [BR, BR, BR])),
        'reject(OR)': reject2D(R.eq(OR)),
        'filter(BR)': filter2D(R.eq(BR)),
        'filter(YE)': filter2D(R.eq(YE)),
    };

    R.forEach(function(fid) {
        $('#available').append('<li id="' + fid + '">' + fid + '</li>');
    }, R.keys(functions));

    $('.sortable').sortable({
        connectWith: '.sortable'
    }).bind('sortupdate', function(e, ui) {
        var ids = [];
        $('#program').find('li').each(function() {
            ids.push($(this).attr('id'));
        });

        var fs = R.map(R.propOf(functions), ids);
        isomer.canvas.clear();
        render(transformationSteps(init, fs));
    });
};
