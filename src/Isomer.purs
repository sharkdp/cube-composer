module Isomer where

import Prelude
import Control.Monad.Eff

foreign import data Isomer :: !

foreign import data IsomerInstance :: *

foreign import data IsomerColor :: *

-- | (Install and) return the Isomer instance on the canvas with the given id
foreign import getIsomerInstance :: forall eff. String
           -> Eff (isomer :: Isomer | eff) IsomerInstance

-- | Render a colored block of size dx, dy, dz at position x, y, z
foreign import renderBlock :: forall eff. IsomerInstance
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
foreign import clearCanvas :: forall eff. IsomerInstance
                           -> Eff (isomer :: Isomer | eff) Unit

-- | Set Isomer scale factor and origin (X and Y)
foreign import setIsomerConfig :: forall eff. IsomerInstance
                               -> Number
                               -> Number
                               -> Number
                               -> Eff (isomer :: Isomer | eff) Unit

-- | Create Isomer.Color object from its RGB representation
foreign import colorFromRGB :: Int
                            -> Int
                            -> Int
                            -> IsomerColor
