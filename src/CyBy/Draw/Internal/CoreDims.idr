module CyBy.Draw.Internal.CoreDims

import Data.Nat
import Text.Measure

%default total

||| Core drawing settings used for computing the geometric properties of
||| drawn molecules.
public export
record CoreDims where
  [noHints]
  constructor CD
  radiusAtom        : Double
  bondWidth         : Double
  bondBGWidth       : Double
  selectBufferSize  : Double
  angleSteps        : Nat
  font              : String
  fontSize          : Nat
  subscriptSize     : Nat

  ||| Gap between two bars of a downward facing wedge
  downWedgeGap      : Double

  ||| Width of the narrow end of a wedge
  wedgeNarrowEnd    : Double

  ||| Width of the wide end of a wedge
  wedgeWideEnd      : Double

  ||| Half the wavelength of a wavy bond
  halfWaveLength    : Double

  ||| Amplitude of a wavy bond
  waveAmplitude     : Double

  ||| Utility used for measuring text
  measure           : Measure

  0 stepsPrf        : IsSucc angleSteps

export
defaultCore : CoreDims
defaultCore =
  CD
    { radiusAtom        = 5.0
    , bondWidth         = 1.0
    , bondBGWidth       = 4.0
    , selectBufferSize  = 10.0
    , angleSteps        = 24
    , stepsPrf          = ItIsSucc
    , font              = "Arial, Helvetica, 'Liberation Sans', sans-serif"
    , fontSize          = 11
    , subscriptSize     = 7
    , downWedgeGap      = 2.0
    , wedgeNarrowEnd    = 0.1
    , wedgeWideEnd      = 5
    , halfWaveLength    = 2.5
    , waveAmplitude     = 1.5
    , measure           = defaultMeasure
    }
