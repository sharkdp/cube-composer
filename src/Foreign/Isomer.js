// module Isomer

exports.getIsomerInstance = (function () {
  var instances = {};

  return function(canvasId) {
    return function() {
      if (!instances.hasOwnProperty(canvasId)) {
        var canvas = document.getElementById(canvasId);
        instances[canvasId] = new Isomer(canvas);
      }
      return instances[canvasId];
    };
  };
})();

exports._renderBlock = function (isomer, x, y, z, dx, dy, dz, color) {
  return function() {
    isomer.add(
      new Isomer.Shape.Prism(new Isomer.Point(x, y, z), dx, dy, dz),
      color
      );
    return {};
  };
};

exports.clearCanvas = function (isomer) {
  return function() {
    isomer.canvas.clear();
    return {};
  };
};

exports._setIsomerConfig = function (isomer, scale, originX, originY) {
  return function() {
    isomer.scale = scale;
    isomer.originX = originX;
    isomer.originY = originY;
    isomer._calculateTransformation();
    return {};
  };
};

exports._colorFromRGB = function (r, g, b) {
  return new Isomer.Color(r, g, b);
};
