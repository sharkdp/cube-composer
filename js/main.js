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
        this.xmax = 4;
        this.ymax = 13;

        _.bindAll(this);
    };

    Game.prototype.renderBlock = function(x, y, value, z) {
        this.iso.add(
            new Shape.Prism(
                new Point(4.2 * (this.xmax - x), this.ymax - y, z),
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
        _.each(lines.reverse(), this.renderLine);
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

        var _deepmap = _.curry(function(f, xs) {
            return _.map(xs, function(x) {
                if (_.isArray(x)) {
                    return _.map(x, f);
                }
                return f(x);
            });
        });
        var _deepflatmap = _.curry(function(f, xs) {
            return _.flatten(_deepmap(f)(xs));
        });
        var _deepfilter = _.curry(function(f, xs) {
            return _.reduce(xs, function(ys, x) {
                if (_.isArray(x)) {
                    var xf = _.filter(x, f);
                    if (xf.length > 0) {
                        ys.push(xf);
                    }
                } else {
                    if (f(x)) {
                        ys.push(x);
                    }
                }
                return ys;
            }, []);
        });

        var cleanup = _map(function(x) {
            if (_.isArray(x)) {
                if (x.length === 1) {
                    return x[0];
                }
                return _.flattenDeep(x);
            }
            return x;
        });

        var randInt = function(range) {
            return Math.floor(range * Math.random());
        };

        var randomColor = function() {
            return _.sample([BL, RE]);
        };

        var randomArgs = function(f, nArgs) {
            var arg = randomColor();
            if (nArgs === 1) {
                return f(arg);
            }
            return randomArgs(f(arg), nArgs - 1);
        };

        var randomF = function() {
            var i1, f1, f2;
            i1 = randInt(10);
            if (i1 < 8) { // map or flatmap
                f1 = _.sample([_deepmap, _deepflatmap]);
                f2 = _.sample([
                    randomArgs(a2b, 2),
                    randomArgs(a2bc, 3)
                ]);
                return f1(f2);
            } // filter
            f2 = _.sample([equal, notEqual]);
            return _deepfilter(randomArgs(f2, 1));
        };

        var queue = [
            _flatmap(clone),
            // _deepflatmap(a2bc(BR, BR, YE)),
            _deepmap(a2bc(BR, BR, YE)),
            _deepmap(a2bc(BR, BR, YE)),
            _deepfilter(equal(YE))
            // _flatmap(a2bc(YE, YE, BR)),
            // _map(a2bc(BR, BR, YE)),
            // _flatmap(blueToBD),
            // _filter(notEqual(1)),
            // _map(clone),
        ];

        var initial = [BR, YE, BR];

        // queue = [ randomF(), randomF(), randomF(), randomF(), randomF() ];
        // initial = [ randomColor(), randomColor(), randomColor() ];

        var lines = _.reduce(queue, function(ls, func) {
            var newLine = cleanup(func(_.last(ls)));
            ls.push(newLine);
            return ls;
        }, [initial]);

        _.each(lines, function(l) {console.log(l);});

        game.renderLines(lines);
    };
}());
