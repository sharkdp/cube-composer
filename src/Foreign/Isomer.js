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

exports.renderBlock = function (isomer) {
  return function(x) { return function(y) { return function(z) {
  return function(dx) { return function(dy) { return function(dz) {
  return function(color) { return function() {
    isomer.add(
      new Isomer.Shape.Prism(new Isomer.Point(x, y, z), dx, dy, dz),
      color
      );
    return {};
  }; }; }; }; }; }; }; };
};

exports.clearCanvas = function (isomer) {
  return function() {
    isomer.canvas.clear();
    return {};
  };
};

exports.setIsomerConfig = function (isomer) {
  return function(scale) {
    return function(originX) {
      return function(originY) {
        return function() {
          isomer.scale = scale;
          isomer.originX = originX;
          isomer.originY = originY;
          isomer._calculateTransformation();
          return {};
        };
      };
    };
  };
};

exports.colorFromRGB = function (r) {
  return function(g) {
    return function(b) {
      return new Isomer.Color(r, g, b);
    };
  };
};
