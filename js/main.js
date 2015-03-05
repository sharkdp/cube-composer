/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global _, Isomer*/

(function () {
    "use strict";

    // flatMap = flatten . map (aka concatMap)
    _.mixin({
        flatMap: _.compose(_.flatten, _.map)
    });

    var Shape = Isomer.Shape;
    var Point = Isomer.Point;
    var Color = Isomer.Color;

    // http://www.colourlovers.com/palette/148712/Gamebookers
    var colors = {
        orange: new Color(255, 153, 0),
        dark: new Color(66, 66, 66),
        light: new Color(233, 233, 233),
        gray: new Color(188, 188, 188),
        blue: new Color(50, 153, 187)
    };

    var colorLine = [
        colors.orange,
        colors.dark,
        colors.light,
        colors.gray,
        colors.blue
    ];

    var Game = function() {
        this.canvas = document.getElementById("canvas");
        this.iso = new Isomer(this.canvas);
        this.iso.scale = 60;
        this.ymax = 10;

        _.bindAll(this);
    };

    Game.prototype.renderBlock = function(z, x, value, y) {
        this.iso.add(
            new Shape.Prism(
                new Point(2.2 * x, this.ymax - y, z),
                1, 1, 1
            ),
            colorLine[value]
        );
    };

    Game.prototype.renderLine = function(line, x) {
        _.each(line, _.partial(this.renderBlock, 0, x));
    };

    Game.prototype.renderLines = function(lines) {
        _.each(lines, this.renderLine);
    };

    window.onload = function() {
        var game = new Game();

        // var isOrange = function(x) { return x === 0; };
        // var isBlue = function(x) { return x === 4; };
        var isDark = function(x) { return x === 1; };
        var blueToBD = function(x) { return (x === 4) ? [4, 1] : [x]; };
        var clone = function(x) { return [x, x]; };

        var _map = _.curryRight(_.map, 2);
        var _flatmap = _.curryRight(_.flatMap, 2);
        var _filter = _.curryRight(_.filter, 2);

        var queue = [
            _flatmap(blueToBD),
            _flatmap(clone),
            _flatmap(blueToBD),
            _filter(isDark),
        ];

        var initial = [4, 4];

        var lines = _.reduce(queue, function(ls, f) {
            ls.push(f(_.last(ls)));
            return ls;
        }, [initial]);

        game.renderLines(lines);
    };
}());
