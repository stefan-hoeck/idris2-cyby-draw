module CyBy.Draw.PeriodicTableCanvas

import Chem
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Draw
import CyBy.Draw.Event
import CyBy.Draw.Internal.Settings
import Data.Finite
import Data.List
import Data.Nat
import Data.String
import Derive.Prelude
import Geom
import Text.SVG
import Text.SVG.Attribute as A

%default total
%language ElabReflection

||| Dimensions of the SVG element (its `width` and `height`).
public export
record SceneDims where
  constructor SD
  swidth  : Double
  sheight : Double

||| Center of the SVG scene.
export
sceneCenter : SceneDims -> Point Id
sceneCenter sd = P (sd.swidth / 2.0) (sd.sheight / 2.0)

||| Bounds of the SVG scene.
export
sceneBounds : SceneDims -> Bounds2D Id
sceneBounds sd = BS (range 0.0 sd.swidth) (range 0.0 sd.sheight)

--------------------------------------------------------------------------------
--          PSE Cells
--------------------------------------------------------------------------------

public export
record Cell where
  constructor PC
  element  : Elem
  ||| 0-based x-position in the periodic table.
  posX     : Nat

  ||| 0-based y-position in the periodic table.
  posY     : Nat

%runElab derive "Cell" [Show,Eq]

xRelativeTo : Elem -> Elem -> Nat
xRelativeTo x rel = cast $ conIndexElem x - conIndexElem rel

public export
PSERows, PSEColumns : Nat
PSERows    = 10
PSEColumns = 18

elemPosition : Elem -> (Nat,Nat)
elemPosition H  = (0,0)
elemPosition He = (17,0)
elemPosition Li = (0,1)
elemPosition Be = (1,1)
elemPosition Na = (0,2)
elemPosition Mg = (1,2)
elemPosition e  =
  if      e >= B  && e <= Ne then ((e `xRelativeTo` B) + 12,1)-- right part of 2nd period
  else if e >= Al && e <= Ar then ((e `xRelativeTo` Al) + 12,2)-- right part of 3rd period
  else if e >= K  && e <= Kr then (e  `xRelativeTo` K ,3)-- 4th period
  else if e >= Rb && e <= Xe then (e  `xRelativeTo` Rb ,4)-- 5th period
  else if e >= Cs && e <= La then (e  `xRelativeTo` Cs ,5)-- left part of 6th period
  else if e >= Ce && e <= Lu then (e  `xRelativeTo` Ba ,8)-- lanthanides
  else if e >= Hf && e <= Rn then ((e `xRelativeTo` Hf) + 3,5)-- right part of 6th period
  else if e >= Fr && e <= Ac then (e  `xRelativeTo` Fr ,6)-- left part of 7th period
  else if e >= Th && e <= Lr then (e  `xRelativeTo` Ra ,9)-- actinides
  else if e >= Rf && e <= Ts then ((e `xRelativeTo` Rf) + 3,6)-- right part of 7th period without Og
  else (17,6)-- Og

public export
0 Cells : Type
Cells = List Cell

-- Generate a list of cells from the list of chemical elements
-- (plus some info about the size of the canvas)
public export
cells : Cells
cells =
  map (\e => let (x,y) := elemPosition e in PC e x y) values

--------------------------------------------------------------------------------
--          Canvas Output
--------------------------------------------------------------------------------

half : Double
half = 0.5

hcell : Double
hcell = 14.0

wcell : Double
wcell = 21.0

parameters {auto s : DrawSettings}
           (sd     : SceneDims)

  hcellRel : Double
  hcellRel = sd.sheight / cast PSERows

  wcellRel : Double
  wcellRel = sd.swidth / cast PSEColumns

  -- compute the position of the mouse in the PSE grid
  -- this assume that the event was fired from the `HTMLCanvasElement`
  -- we use for drawing the PSE
  mousePos : (x,y : Double) -> (Nat,Nat)
  mousePos x y = (cast $ x / wcellRel, cast $ y / hcellRel)
  
  export
  hoveredElem : {0 t : _} -> Point t -> Maybe Elem
  hoveredElem (P dx dy) =
    let (x,y) := mousePos dx dy
     in element <$> find (\c => c.posX == x && c.posY == y) cells
  
  drawCell : Maybe Elem -> Cell -> SVGNode
  drawCell me (PC elem px py) =
    let x      := cast px * wcell
        y      := cast py * hcell
        txtX   := x + half * wcell
        txtY   := y + half * hcell
        hovCol := if me == Just elem then s.hoverBG else s.defaultBG
     in g [transform (Scale (wcellRel / wcell) (hcellRel / hcell))]
          [ rect [A.x x.u, A.y y.u, width wcell.u, height hcell.u, fill hovCol, stroke black]
          , text1 [A.x txtX.u, A.y txtY.u, fill (s.elemColor elem)] (show elem)
          ]

  export
  displayPSE : Maybe Elem -> SVGNode
  displayPSE me =
    g
      [ fontFamily s.core.font
      , fontSize (cast s.pseFontSize).px
      , textAnchor Middle
      , dominantBaseline Central
      ]
      (map (drawCell me) cells)
