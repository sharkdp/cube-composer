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

var addColorBlocks = function(str) {
    R.forEach(function(cstr) {
        str = str.replace(new RegExp('\{' + cstr + '\}', 'g'), '<div class="rect ' + cstr + '"> </div>');
    }, ['X', 'BL', 'BR', 'RE', 'OR', 'YE'])
    return str;
}

var functions = {
    'stackEqual': stackEqual,
    'flatten': flattenDeep,
    'map({X} ↦ {X}{X})': map2D(clone),
    'map({OR} ↦ {YE})': map2D(replace(OR, YE)),
    'map({YE} ↦ {BR})': map2D(replace(YE, BR)),
    'map({YE} ↦ {BR}{YE})': map2D(replace(YE, [BR, YE])),
    'map({BR} ↦ {BR}{BR}{BR})': map2D(replace(BR, [BR, BR, BR])),
    'map({BR} ↦ {OR}{OR})': map2D(replace(BR, [OR, OR])),
    'reject({OR})': reject2D(R.eq(OR)),
    'reject({YE})': reject2D(R.eq(YE)),
    'filter({BR})': filter2D(R.eq(BR)),
};

// see http://stackoverflow.com/a/11935263/704831
function getRandomSubarray(arr, size) {
    var shuffled = arr.slice(0), i = arr.length, temp, index;
    while (i--) {
        index = Math.floor((i + 1) * Math.random());
        temp = shuffled[index];
        shuffled[index] = shuffled[i];
        shuffled[i] = temp;
    }
    return shuffled.slice(0, size);
}

window.onload = function() {
    var canvas = document.getElementById('canvas');
    isomer = new Isomer(canvas);
    isomer.scale = 40;

    var randFs = getRandomSubarray(R.values(functions), 4);
    var target = R.last(transformationSteps(init, randFs));

    // render([target]);
    render([init]);

    R.forEach(function(fid) {
        $('#available').append('<li id="' + fid + '">' + addColorBlocks(fid) + '</li>');
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
        // render([target]);
        render(transformationSteps(init, fs));
    });
};
