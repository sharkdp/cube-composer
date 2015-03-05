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

    window.onload = function() {
        var canvas = document.getElementById("canvas");
        var iso = new Isomer(canvas);
        iso.scale = 60;

        iso.add(new Shape.Prism(Point.ORIGIN, 4, 5, 1), colors.dark);
        iso.add(new Shape.Prism(new Point(1, 1, 1), 2, 3, 1), colors.light);
        iso.add(new Shape.Prism(new Point(1, 1, 2), 2, 3, 1), colors.gray);
        iso.add(Shape.Prism(new Point(2, 1, 3)), colors.orange);
        iso.add(Shape.Pyramid(new Point(1, 3, 3)), colors.blue);
    };
}());
