module Isomer where

import Control.Monad.Eff

foreign import data Isomer :: !

foreign import data IsomerInstance :: *

foreign import data IsomerColor :: *

-- | (Install and) return the Isomer instance on the canvas with the given id
foreign import getIsomerInstance
  """
  var getIsomerInstance = (function() {
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
  })();""" :: forall eff. String
           -> Eff (isomer :: Isomer | eff) IsomerInstance

-- | Render a colored block of size dx, dy, dz at position x, y, z
foreign import renderBlock
  """
  function renderBlock(isomer) {
    return function(x) { return function(y) { return function(z) {
    return function(dx) { return function(dy) { return function(dz) {
    return function(color) { return function() {
      isomer.add(
        new Isomer.Shape.Prism(new Isomer.Point(x, y, z), dx, dy, dz),
        color
        );
      return {};
    }; }; }; }; }; }; }; };
  }
  """ :: forall eff. IsomerInstance
      -> Number
      -> Number
      -> Number
      -> Number
      -> Number
      -> Number
      -> IsomerColor
      -> Eff (isomer :: Isomer | eff) Unit

-- | Render a single colored cube at the given position
renderCube :: forall eff. IsomerInstance
           -> Number
           -> Number
           -> Number
           -> IsomerColor
           -> Eff (isomer :: Isomer | eff) Unit
renderCube isomer x y z col = renderBlock isomer x y z 0.9 0.9 0.9 col

-- | Clear the whole canvas that belongs to the Isomer instance
foreign import clearCanvas
  """
  function clearCanvas(isomer) {
    return function() {
      isomer.canvas.clear();
      return {};
    };
  }
  """ :: forall eff. IsomerInstance
      -> Eff (isomer :: Isomer | eff) Unit

-- | Set Isomer scale factor and origin (X and Y)
foreign import setIsomerConfig
  """
  function setIsomerConfig(isomer) {
    return function(scale) {
      return function(originX) {
        return function(originY) {
          return function() {
            isomer.scale = scale;
            isomer.originX = originX;
            isomer.originY = originY;
            isomer._calculateTransformation();
            return {};
          }
        };
      };
    };
  }""" :: forall eff. IsomerInstance
       -> Number
       -> Number
       -> Number
       -> Eff (isomer :: Isomer | eff) Unit

-- | Create Isomer.Color object from its RGB representation
foreign import colorFromRGB
  """
  function colorFromRGB(r) {
    return function(g) {
      return function(b) {
        return new Isomer.Color(r, g, b);
      };
    };
  }
  """ :: Number
      -> Number
      -> Number
      -> IsomerColor
