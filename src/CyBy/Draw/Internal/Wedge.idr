module CyBy.Draw.Internal.Wedge

import Geom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Label
import Text.Molfile
import Text.SVG

%default total
%hide Geom.Scale.(/)

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

lineT : Point Id -> Point Id -> Maybe AffineTransformation
lineT s e = (\phi => AT (rotation phi) (s - origin)) <$> angle (e - s)

%inline
transform : (t : AffineTransformation) -> Point Id -> Point t
transform t = convert

--------------------------------------------------------------------------------
-- Wedges
--------------------------------------------------------------------------------

parameters {auto cd : CoreDims}

  -- gap : initial gap (half the remaining distance)
  -- bw  : total bar width (stroke plus gap)
  -- tot : total number of bars
  -- x   : index of current bar
  downCmds : (gap,bw,tot : Double) -> AffineTransformation -> Double -> List PathCmd
  downCmds gap bw tot t x =
    let dw      := cd.wedgeWideEnd - cd.wedgeNarrowEnd
        l       := cd.wedgeNarrowEnd + x * dw / tot -- current bar length
        px      := x * bw + gap
        P x1 y1 := transform t $ P px (l/2.0)
        P x2 y2 := transform t $ P px (l/(-2.0))
     in [M x1 y1, L x2 y2]

  ||| Generates evenly distributed bars for a downward wedge
  ||| beween the two points.
  |||
  ||| This is a sequence of lines in an SVG `<path>`.
  export
  wedgeDown : (start, end : Point Id) -> List PathCmd
  wedgeDown s e =
    let Just t   := lineT s e | Nothing => []
        len      := distance s e -- distance between points
        bw       := cd.downWedgeGap + cd.bondWidth -- total bar width
        True     := len >= cd.bondWidth | False => []
        tot      := floor $ (len - cd.bondWidth) / bw
        gap      := (len - tot * bw) / 2.0
     in [0..cast tot] >>= downCmds gap bw (1.0 + tot) t . cast

  ||| Generates a wedged bond as a polygon.
  export
  wedgeUp : (s,e : Point Id) -> List (SVGAttribute "polygon") -> SVGNode
  wedgeUp s e as =
    let Just t := lineT s e | Nothing => Empty
        len    := distance s e
        P a b  := transform t $ P 0 (cd.wedgeNarrowEnd / 2.0)
        P c d  := transform t $ P 0 (cd.wedgeNarrowEnd / (-2.0))
        P e f  := transform t $ P len (cd.wedgeWideEnd / (-2.0))
        P g h  := transform t $ P len (cd.wedgeWideEnd / 2.0)
     in polygon (points [a,b,c,d,e,f,g,h] :: as)

--------------------------------------------------------------------------------
-- Waved (zigzag) Bond
--------------------------------------------------------------------------------

  waves :
       SnocList PathCmd
    -> (gap : Double)
    -> (pos : Bool)
    -> AffineTransformation
    -> List Nat
    -> List PathCmd
  waves sp gap pos t (a::tl) =
    let P xa ya := transform t $ P (gap + cast a * cd.halfWaveLength) 0
        cmd     := A cd.waveAmplitude cd.waveAmplitude 0 False pos xa ya
     in waves (sp :< cmd) gap (not pos) t tl
  waves sp _ _ _ [] = sp <>> []

  ||| Computes a "wavy" bond as a sequence of arcs on an SVG `<path>`.
  export
  wave : (start,end : Point Id) -> List PathCmd
  wave s e =
    let Just t := lineT s e | Nothing => []
        len    := distance s e
        True   := len >= cd.halfWaveLength | False => []
        tot    := floor (len / cd.halfWaveLength)
        gap    := len - tot * cd.halfWaveLength
        P x y  := transform t $ P gap 0 
     in waves [<M x y] gap True t [1..cast tot]
