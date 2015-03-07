/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global _, Isomer*/

(function () {
    "use strict";

    // flatMap (aka concatMap)
    _.mixin({
        flatMap: _.compose(_.flatten, _.map)
    });

    var Shape = Isomer.Shape;
    var Point = Isomer.Point;
    var Color = Isomer.Color;
    var Path = Isomer.Path;

    // http://www.colourlovers.com/palette/148712/Gamebookers
    // var colorLine = [
    //     new Color(255, 153, 0),
    //     new Color(66, 66, 66),
    //     new Color(233, 233, 233),
    //     new Color(188, 188, 188),
    //     new Color(50, 153, 187)
    // ];

    // http://www.colourlovers.com/palette/1473/Ocean_Five
    var colorLine = [
        new Color(0, 160, 176),
        new Color(106, 74, 60),
        new Color(204, 51, 63),
        new Color(235, 104, 65),
        new Color(237, 201, 81)
    ];

    var Game = function() {
        this.canvas = document.getElementById("canvas");
        this.iso = new Isomer(this.canvas);
        this.iso.scale = 54;
        this.ymax = 13;

        _.bindAll(this);
    };

    Game.prototype.renderBlock = function(x, y, value, z) {
        this.iso.add(
            new Shape.Prism(
                new Point(3.2 * x, this.ymax - y, z),
                1, 1, 1
            ),
            colorLine[value]
        );
    };

    Game.prototype.renderBlocks = function(x, value, y) {
        if (_.isArray(value)) {
            _.each(value, _.partial(this.renderBlock, x, y));
        } else {
            this.renderBlock(x, y, value, 0);
        }
    };

    Game.prototype.renderLine = function(line, x) {
        _.each(line, _.partial(this.renderBlocks, x));
    };

    Game.prototype.renderLines = function(lines) {
        _.each(lines, this.renderLine);
    };

    window.onload = function() {
        var game = new Game();

        var BL = 0;
        var BR = 1;
        var RE = 2;
        var OR = 3;
        var YE = 4;

        var equal = _.curry(function(x, y) { return x === y; });
        var notEqual = _.curry(function(x, y) { return x !== y; });

        var a2b = _.curry(function(a, b, x) {
            return (x === a) ? b : x;
        });
        var a2bc = _.curry(function(a, b, c, x) {
            return (x === a) ? [b, c] : x;
        });

        var clone = function(x) { return [x, x]; };

        var _map = _.curryRight(_.map, 2);
        var _flatmap = _.curryRight(_.flatMap, 2);
        var _filter = _.curryRight(_.filter, 2);

        var cleanup = _map(function(x) {
            if (_.isArray(x)) {
                if (x.length === 1) {
                    return x[0];
                }
                return _.flattenDeep(x);
            }
            return x;
        });

        var queue = [
            // _flatmap(blueToBD),
            // _flatmap(clone),
            _flatmap(a2bc(YE, YE, BR)),
            _map(a2bc(BR, BR, YE)),
            // _flatmap(blueToBD),
            // _filter(notEqual(1)),
            _map(clone),
        ];

        var initial = [BR, YE, YE];

        var lines = _.reduce(queue, function(ls, func) {
            var newLine = cleanup(func(_.last(ls)));
            ls.push(newLine);
            return ls;
        }, [initial]);

        _.each(lines, function(l) {console.log(l);});

        game.renderLines(lines);
    };
}());
