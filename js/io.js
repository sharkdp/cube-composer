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
    ymax: 16,
    xmax: 7
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
    R.forEachIndexed(renderLine, steps);
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
    'map(push({YE}))': R.map(R.append(YE)),
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

var renderTarget = function(target) {
    // TODO: this is ugly

    var originX = isomer.originX,
        originY = isomer.originY,
        scale = isomer.scale;
    isomer.originX = 10;
    isomer.originY = 1000;
    isomer.scale = 30;

    render([target]);

    isomer.originX = originX;
    isomer.originY = originY;
    isomer.scale = scale;
};

var newPuzzle = function() {
    // new initial state
    // init = wrap(getRandomSubarray([OR, BR, BR, BR, BR, BR, BR, BR, YE, YE, YE, YE], 6));
    init = wrap([BR, OR, OR, YE, YE, YE, OR, OR, BR]);
    console.log(init);

    // new puzzle
    var randFs = getRandomSubarray(R.values(functions), 5);
    target = R.last(transformationSteps(init, randFs));
};

var renderAll = function() {
    var ids = [];
    $('#program').find('li').each(function() {
        ids.push($(this).attr('id'));
    });

    var fs = R.map(R.prop(R.__, functions), ids);
    isomer.canvas.clear();
    renderTarget(target);
    render(transformationSteps(init, fs));
}

var liClickHandler = function() {
    var $li = $(this);
    var targetId = 'program';
    if ($li.parent().attr('id') == 'program') {
        targetId = 'available';
    }
    $li.remove();
    $li.on('click', liClickHandler);
    $('#' + targetId).append($li);

    renderAll();
    resetSortable();
}

var resetSortable = function() {
    $('.sortable').each(function() {
        new Sortable(this, {
            group: 'function-lists',
            ghostClass: 'sortable-ghost',
            animation: 150,
            onSort: renderAll
        });
    });
}

var resetControls = function() {
    $('.sortable').empty();
    R.forEach(function(fid) {
        var $li = $('<li id="' + fid + '">' + addColorBlocks(fid) + '</li>');
        $li.on('click', liClickHandler);
        $('#available').append($li);
    }, R.keys(functions));

    resetSortable();
};

var init;
var target;

window.onload = function() {
    var canvas = document.getElementById('canvas');
    isomer = new Isomer(canvas);
    isomer.scale = 40;

    newPuzzle();
    resetControls();
    renderAll();

    $(window).on('keypress', function(ev) {
        if (ev.keyCode === 114) { // r: reset program
            resetControls();
            renderAll();
        } else if (ev.keyCode === 103) { // g: generate new puzzle
            newPuzzle();
            renderAll();
        }
    });
};
