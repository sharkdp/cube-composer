module Isomer where

import Control.Monad.Eff

foreign import data Isomer :: !

foreign import data IsomerInstance :: *

foreign import data IsomerColor :: *

foreign import getIsomerInstance """
  function getIsomerInstance(canvasId) {
    return function() {
      var canvas = document.getElementById(canvasId);
      return new Isomer(canvas);
    };
  }""" :: forall eff. String -> Eff (isomer :: Isomer | eff) IsomerInstance

foreign import renderCube """
  function renderCube(isomer) {
    return function(x) {
      return function(y) {
        return function(z) {
          return function(color) {
            return function() {
              isomer.add(
                new Isomer.Shape.Prism(
                  new Isomer.Point(x, y, z),
                  1, 1, 1
                ),
                color
              );
              return isomer;
            };
          }
        }
      };
    };
  }""" :: forall eff. IsomerInstance -> Number -> Number -> Number -> IsomerColor -> Eff (isomer :: Isomer | eff) IsomerInstance

foreign import clearCanvas """
  function clearCanvas(isomer) {
    return function() {
      isomer.canvas.clear();
      return isomer;
    };
  }""" :: forall eff. IsomerInstance -> Eff (isomer :: Isomer | eff) IsomerInstance

foreign import setIsomerConfig """
  function setIsomerConfig(isomer) {
    return function(scale) {
      return function(originX) {
        return function(originY) {
          return function() {
            isomer.scale = scale;
            isomer.originX = originX;
            isomer.originY = originY;
            // This will be needed for isomer >= 0.2.4
            // isomer._calculateTransformation();
            return isomer;
          }
        };
      };
    };
  }""" :: forall eff. IsomerInstance -> Number -> Number -> Number -> Eff (isomer :: Isomer | eff) IsomerInstance


foreign import colorFromRGB """
    function colorFromRGB(r) {
        return function(g) {
            return function(b) {
                return new Isomer.Color(r, g, b);
            };
        };
    }""" :: Number -> Number -> Number -> IsomerColor
