module CanvasExtra
  ( getFillStyle
  ) where

import Graphics.Canvas (Context2D)

foreign import getFillStyle :: Context2D -> String
