module CyBy.Draw.Internal.Settings

import CyBy.Draw.Internal.Abbreviations
import CyBy.Draw.Internal.Color
import CyBy.Draw.Internal.CoreDims
import Text.SVG
import Chem
import Geom

%default total

--------------------------------------------------------------------------------
--          Settings
--------------------------------------------------------------------------------

public export
record DrawSettings where
  [noHints]
  constructor MS
  core              : CoreDims
  abbreviations     : List Abbreviation
  bondColor         : SVGColor
  defaultBG         : SVGColor
  errorBG           : SVGColor
  highlightBG       : SVGColor
  hoverBG           : SVGColor
  newBG             : SVGColor
  originBG          : SVGColor
  selectBG          : SVGColor
  selectFG          : SVGColor
  showC             : Bool
  textColor         : SVGColor
  elemColor         : Elem -> SVGColor -- color scheme to use
  maxZoom           : Scale
  minZoom           : Scale
  pseFontSize       : Nat
  resizeCornerRad   : Double

export
defaultSettings : List Abbreviation -> DrawSettings
defaultSettings as =
  MS {
     abbreviations     = as
   , core              = defaultCore
   , hoverBG           = RGBA 71 112 204 50.perc
   , originBG          = RGB 132 197 98
   , newBG             = lightgrey
   , selectBG          = RGBA 132 197 98 50.perc
   , selectFG          = RGBA 132 197 98 50.perc
   , defaultBG         = white
   , highlightBG       = lightgreen
   , errorBG           = RGB 255 150 150
   , textColor         = black
   , elemColor         = basicColors
   , bondColor         = black
   , showC             = False
   , maxZoom           = 20
   , minZoom           = 0.1
   , pseFontSize       = 11
   , resizeCornerRad   = 20.0
   }

export %inline %hint
toCoreDims : (ds : DrawSettings) => CoreDims
toCoreDims = ds.core

export
(.selectBufferV) : DrawSettings -> Vector Id
s.selectBufferV = vid s.core.selectBufferSize s.core.selectBufferSize

||| Makes sure the given scaling factor does not exceed the valid
||| zoom levels.
export
validScale : (s : DrawSettings) => AffineTransformation -> Scale -> Scale
validScale t sc =
  let tot      := sc * t.transform.scale -- new scaling factor we would get
      validTot := min (max tot s.minZoom) s.maxZoom -- valid scaling factor
      False    := validTot == tot | True => sc
   in sc * scale (validTot.value / tot.value) -- adjust scale if result would be out of bounds
