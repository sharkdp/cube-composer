/*jslint browser: true, nomen: true, white: true, vars: true*/
/*global _, Isomer*/

(function () {
    "use strict";

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
    };

    Game.prototype.renderLine = function(data, x) {
        _.map(data, function(p, y) {
            this.iso.add(new Shape.Prism(new Point(x, this.ymax - y, 0), 1, 1, 1), colorLine[p]);
        }, this);
    };

    window.onload = function() {
        var game = new Game();

        game.renderLine([0, 1, 2, 3, 2, 1, 4], 0);
    };
}());
