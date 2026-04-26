module CyBy.Draw

import Data.Finite
import Data.List
import Geom
import Text.CSS
import Text.HTML.DomID
import Text.HTML.Select
import Text.Show.Pretty
import Text.SVG
import Web.Async
import Web.Internal.Types

import CyBy.Draw.Internal.Color
import CyBy.Draw.Internal.Label
import CyBy.UI.CSS.Classes
import Geom.Gen2D.Debug

import public CyBy.Draw.Draw
import public CyBy.Draw.Event
import public CyBy.Draw.Internal.Abbreviations
import public CyBy.Draw.Internal.Atom
import public CyBy.Draw.Internal.CoreDims
import public CyBy.Draw.Internal.Graph
import public CyBy.Draw.Internal.Ring
import public CyBy.Draw.Internal.Role
import public CyBy.Draw.Internal.Settings
import public CyBy.Draw.MoleculeCanvas
import public CyBy.Draw.PeriodicTableCanvas
import public Text.Molfile

%default total
%hide Text.SVG.Types.Path.t

--------------------------------------------------------------------------------
-- Icons
--------------------------------------------------------------------------------

select : HTMLNode
select = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 3.332031 16.667969 L 3.332031 17.5 L 1.667969 17.5 L 1.667969 15.832031 L 2.5 15.832031 L 2.5 16.667969 Z M 2.5 2.5 L 3.332031 2.5 L 3.332031 1.667969 L 1.667969 1.667969 L 1.667969 3.332031 L 2.5 3.332031 Z M 1.667969 6.667969 L 2.5 6.667969 L 2.5 5 L 1.667969 5 Z M 1.667969 10.832031 L 2.5 10.832031 L 2.5 8.332031 L 1.667969 8.332031 Z M 16.667969 6.667969 L 17.5 6.667969 L 17.5 5 L 16.667969 5 Z M 16.667969 10 L 17.5 10 L 17.5 8.332031 L 16.667969 8.332031 Z M 1.667969 14.167969 L 2.5 14.167969 L 2.5 12.5 L 1.667969 12.5 Z M 6.667969 2.5 L 6.667969 1.667969 L 5 1.667969 L 5 2.5 Z M 10.832031 2.5 L 10.832031 1.667969 L 8.332031 1.667969 L 8.332031 2.5 Z M 7.5 17.5 L 7.5 16.667969 L 5.832031 16.667969 L 5.832031 17.5 Z M 10.832031 17.5 L 10.832031 16.667969 L 9.167969 16.667969 L 9.167969 17.5 Z M 14.167969 2.5 L 14.167969 1.667969 L 12.5 1.667969 L 12.5 2.5 Z M 15.832031 1.667969 L 15.832031 2.5 L 16.667969 2.5 L 16.667969 3.332031 L 17.5 3.332031 L 17.5 1.667969 Z M 15.628906 18.332031 L 17.867188 17.207031 L 15.9375 13.332031 L 19.167969 13.332031 L 11.667969 7.886719 L 11.667969 17.136719 L 13.652344 14.417969 Z M 13.78125 12.824219 L 12.5 14.582031 L 12.5 9.523438 L 16.601562 12.5 L 14.59375 12.5 L 16.75 16.835938 L 15.996094 17.214844 Z M 13.78125 12.824219'/></svg>"

erase : HTMLNode
erase = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 10.109375 2.757812 C 11.082031 1.78125 12.667969 1.78125 13.640625 2.757812 L 18.492188 7.609375 C 19.46875 8.582031 19.46875 10.167969 18.492188 11.140625 L 11.617188 18.015625 C 11.148438 18.488281 10.511719 18.75 9.847656 18.75 L 6.398438 18.75 C 5.738281 18.75 5.101562 18.488281 4.632812 18.015625 L 1.507812 14.890625 C 0.53125 13.917969 0.53125 12.332031 1.507812 11.359375 L 10.105469 2.757812 Z M 12.757812 3.640625 C 12.269531 3.15625 11.480469 3.15625 10.992188 3.640625 L 5.199219 9.433594 L 11.816406 16.050781 L 17.609375 10.257812 C 18.09375 9.769531 18.09375 8.980469 17.609375 8.492188 Z M 10.933594 16.933594 L 4.316406 10.316406 L 2.390625 12.242188 C 1.90625 12.730469 1.90625 13.519531 2.390625 14.007812 L 5.515625 17.132812 C 5.75 17.367188 6.070312 17.5 6.402344 17.5 L 9.851562 17.5 C 10.179688 17.5 10.5 17.367188 10.734375 17.132812 Z M 10.933594 16.933594'/></svg>"

clear : HTMLNode
clear = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 15.726562 5 L 16.5625 5 L 15.742188 18.347656 C 15.710938 18.808594 15.328125 19.167969 14.867188 19.167969 L 5.132812 19.167969 C 4.671875 19.164062 4.289062 18.808594 4.257812 18.347656 L 3.4375 5 L 4.273438 5 L 5.089844 18.292969 C 5.089844 18.316406 5.109375 18.332031 5.132812 18.332031 L 14.867188 18.332031 Z M 7.917969 15.832031 C 8.148438 15.832031 8.332031 15.648438 8.332031 15.417969 L 8.332031 7.082031 C 8.332031 6.851562 8.148438 6.667969 7.917969 6.667969 C 7.6875 6.667969 7.5 6.851562 7.5 7.082031 L 7.5 15.417969 C 7.5 15.648438 7.6875 15.832031 7.917969 15.832031 Z M 12.082031 15.832031 C 12.3125 15.832031 12.5 15.648438 12.5 15.417969 L 12.5 7.082031 C 12.5 6.851562 12.3125 6.667969 12.082031 6.667969 C 11.851562 6.667969 11.667969 6.851562 11.667969 7.082031 L 11.667969 15.417969 C 11.667969 15.648438 11.851562 15.832031 12.082031 15.832031 Z M 4.21875 4.167969 L 2.5 4.167969 L 2.5 3.332031 L 6.667969 3.332031 L 6.667969 2.707031 C 6.667969 2.132812 7.132812 1.667969 7.707031 1.667969 L 12.292969 1.667969 C 12.867188 1.667969 13.332031 2.132812 13.332031 2.707031 L 13.332031 3.332031 L 17.5 3.332031 L 17.5 4.167969 Z M 7.5 3.332031 L 12.5 3.332031 L 12.5 2.707031 C 12.5 2.59375 12.40625 2.5 12.292969 2.5 L 7.707031 2.5 C 7.59375 2.5 7.5 2.59375 7.5 2.707031 Z M 7.5 3.332031'/></svg>"

undo : HTMLNode
undo = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 6.667969 5.417969 L 6.667969 7.6875 C 7.5 6.800781 8.6875 6.25 10 6.25 C 12.53125 6.25 14.582031 8.300781 14.582031 10.832031 C 14.582031 12.097656 14.070312 13.246094 13.242188 14.074219 L 12.210938 13.042969 C 12.773438 12.476562 13.125 11.695312 13.125 10.832031 C 13.125 9.105469 11.726562 7.707031 10 7.707031 C 9.074219 7.707031 8.242188 8.109375 7.671875 8.75 L 10 8.75 L 8.75 10 L 5.417969 10 L 5.417969 6.667969 Z M 6.667969 5.417969 '/></svg>"

redo : HTMLNode
redo = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 13.332031 5.417969 L 14.582031 6.667969 L 14.582031 10 L 11.25 10 L 10 8.75 L 12.328125 8.75 C 11.757812 8.109375 10.925781 7.707031 10 7.707031 C 8.273438 7.707031 6.875 9.105469 6.875 10.832031 C 6.875 11.695312 7.226562 12.476562 7.789062 13.042969 L 6.757812 14.074219 C 5.929688 13.246094 5.417969 12.097656 5.417969 10.832031 C 5.417969 8.300781 7.46875 6.25 10 6.25 C 11.3125 6.25 12.496094 6.800781 13.332031 7.6875 Z M 13.332031 5.417969 '/></svg>"

center : HTMLNode
center = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 5 1.25 L 1.25 1.25 L 1.25 5 L 2.5 5 L 2.5 2.5 L 5 2.5 Z M 5 1.25 '/><path class='\{fillPath}' d='M 15 1.25 L 18.75 1.25 L 18.75 5 L 17.5 5 L 17.5 2.5 L 15 2.5 Z M 15 1.25 '/><path class='\{fillPath}' d='M 5 18.75 L 1.25 18.75 L 1.25 15 L 2.5 15 L 2.5 17.5 L 5 17.5 Z M 5 18.75 '/><path class='\{fillPath}' d='M 15 18.75 L 18.75 18.75 L 18.75 15 L 17.5 15 L 17.5 17.5 L 15 17.5 Z M 15 18.75 '/><path class='\{fillPath}' d='M 15 15 L 5 15 C 4.308594 15 3.75 14.441406 3.75 13.75 L 3.75 6.25 C 3.75 5.558594 4.308594 5 5 5 L 15 5 C 15.691406 5 16.25 5.558594 16.25 6.25 L 16.25 13.75 C 16.25 14.441406 15.691406 15 15 15 Z M 5 6.25 L 5 13.75 L 15 13.75 L 15 6.25 Z M 5 6.25 '/></svg>"

zoomIn : HTMLNode
zoomIn = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 18.96875 17.0625 L 15.4375 13.53125 C 15.171875 13.265625 14.746094 13.265625 14.484375 13.53125 L 14.300781 13.710938 L 13.082031 12.492188 L 13.242188 12.332031 C 15.601562 9.484375 15.304688 5.289062 12.574219 2.800781 C 9.839844 0.308594 5.636719 0.410156 3.023438 3.023438 C 0.410156 5.636719 0.308594 9.839844 2.800781 12.574219 C 5.289062 15.304688 9.484375 15.601562 12.332031 13.242188 L 12.492188 13.082031 L 13.710938 14.300781 L 13.53125 14.484375 C 13.265625 14.746094 13.265625 15.175781 13.53125 15.4375 L 17.0625 18.96875 C 17.328125 19.230469 17.753906 19.230469 18.015625 18.96875 L 18.96875 18.015625 C 19.230469 17.753906 19.230469 17.328125 18.96875 17.0625 Z M 7.914062 14 C 6.300781 14.003906 4.753906 13.363281 3.617188 12.21875 C 1.292969 9.898438 1.230469 6.148438 3.480469 3.75 C 5.730469 1.355469 9.476562 1.179688 11.941406 3.355469 C 14.40625 5.53125 14.695312 9.269531 12.597656 11.796875 L 11.742188 12.648438 C 10.660156 13.527344 9.308594 14.003906 7.914062 14 Z M 17.539062 18.265625 L 14.230469 14.960938 L 14.960938 14.234375 L 18.269531 17.539062 Z M 8.332031 7.5 L 10.832031 7.5 L 10.832031 8.332031 L 8.332031 8.332031 L 8.332031 10.832031 L 7.5 10.832031 L 7.5 8.332031 L 5 8.332031 L 5 7.5 L 7.5 7.5 L 7.5 5 L 8.332031 5 Z M 8.332031 7.5 '/></svg>"

zoomOut : HTMLNode
zoomOut = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 18.96875 17.0625 L 15.4375 13.53125 C 15.171875 13.265625 14.746094 13.265625 14.484375 13.53125 L 14.300781 13.710938 L 13.082031 12.492188 L 13.242188 12.332031 C 15.601562 9.484375 15.304688 5.289062 12.574219 2.800781 C 9.839844 0.308594 5.636719 0.410156 3.023438 3.023438 C 0.410156 5.636719 0.308594 9.839844 2.800781 12.574219 C 5.289062 15.304688 9.484375 15.601562 12.332031 13.242188 L 12.492188 13.082031 L 13.710938 14.300781 L 13.53125 14.484375 C 13.265625 14.746094 13.265625 15.175781 13.53125 15.4375 L 17.0625 18.96875 C 17.328125 19.230469 17.753906 19.230469 18.015625 18.96875 L 18.96875 18.015625 C 19.230469 17.753906 19.230469 17.328125 18.96875 17.0625 Z M 7.914062 14 C 6.300781 14.003906 4.753906 13.363281 3.617188 12.21875 C 1.292969 9.898438 1.230469 6.148438 3.480469 3.75 C 5.730469 1.355469 9.476562 1.179688 11.941406 3.355469 C 14.40625 5.53125 14.695312 9.269531 12.597656 11.796875 L 11.742188 12.648438 C 10.660156 13.527344 9.308594 14.003906 7.914062 14 Z M 17.539062 18.265625 L 14.230469 14.960938 L 14.960938 14.234375 L 18.269531 17.539062 Z M 5 7.5 L 10.832031 7.5 L 10.832031 8.332031 L 5 8.332031 Z M 5 7.5 '/></svg>"

single : HTMLNode
single = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 2.746094 17.253906 C 2.417969 16.929688 2.417969 16.402344 2.746094 16.078125 L 16.078125 2.746094 C 16.285156 2.527344 16.597656 2.441406 16.886719 2.515625 C 17.179688 2.59375 17.40625 2.820312 17.484375 3.113281 C 17.558594 3.402344 17.472656 3.714844 17.253906 3.921875 L 3.921875 17.253906 C 3.597656 17.582031 3.070312 17.582031 2.746094 17.253906 Z M 2.746094 17.253906 '/></svg>"

double : HTMLNode
double = Raw "<svg viewBox='0 0 20 20'><path class='\{molPath}' d='M 13.9974 4.000678 L 14.000714 20.000073 M 10.000037 4.000678 L 10.000037 19.996759 ' transform='matrix(0.589256,0.589256,-0.589256,0.589256,10,-4)'/></svg>"

triple : HTMLNode
triple = Raw "<svg viewBox='0 0 20 20'><g transform='translate(1.1112556,1.0188408)'> <path class='\{molPath}' d='m 13.9974,4.000678 0.0033,15.999395 M 10.000037,4.000678 v 15.996081' transform='matrix(0.589256,0.589256,-0.589256,0.589256,10,-4)' id='path1'/></g><g transform='translate(-1.3459359,-1.2765732)'><path class='\{molPath}' d='m 13.9974,4.000678 0.0033,15.999395 M 10.000037,4.000678 v 15.996081' transform='matrix(0.589256,0.589256,-0.589256,0.589256,10,-4)'/></g></svg>"

bondUp : HTMLNode
bondUp = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}' d='M 13.82885,1.8834035 1.7076869,18.082345 18.320575,5.7048658 Z'</svg>"

bondDown : HTMLNode
-- down = Raw "<svg viewBox='0 0 20 20'><path class='\{fillPath}'

bondEither : HTMLNode

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

export
color : ColorScheme -> Elem -> SVGColor
color Black  = const black
color CyBy   = basicColors
color Groups = groupColors
color CPK    = cpkColor
color CDK    = cdkColor
color JMol   = jmolColor
color PyMol  = pymolColor

%inline
molToClipboard : HasIO io => CDGraph -> io ()
molToClipboard = toClipboard . writeMolfile . toMolfile

fromClipboard : Sink DrawEvent => Loggable JS DrawMsg => Act ()
fromClipboard =
  readFromClipboard >>= \s =>
    case readMolfileE s of
      Left x  => case smilesToMol s of
        Left  _ => logLoggable (ReadErr x)
        Right m => sink (Event.SetTempl $ initGraph m.graph)
      Right g => sink (Event.SetTempl g)

export
storeSVG : String -> Act ()
storeSVG s =
  use1 (blob s "image/svg+xml" >>= blobURL) $ \u => Prelude.do
    e  <- createElement "a"
    setAttribute e "href" (cast u)
    setAttribute e "download" "cyby_draw_img.svg"
    he <- jsCast {t = HTMLElement} "storeSVG:<a> conversion" e
    click he

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

||| Minimal environment required to run the core of cyby-draw.
public export
record DrawEnv where
  [noHints]
  constructor DE
  pre          : String
  {auto sets   : DrawSettings}
  {auto events : Sink DrawEvent}

||| Extension interface, currently used for the word plugin.
||| If the import button should be used, a tuple with the
||| class and title has to be specified. If no import button
||| is used, this is indicated by a `Nothing`.
||| If the export button should be modified, it also had to
||| be specified.
public export
record Extension where
  [noHints]
  constructor E
  doExport : DrawSettings => DrawState -> Act ()

  ||| Creats additional buttons to be displayed in the top bar
  ||| These might require additional mutable state (for instance, the current
  ||| colour scheme) , so this is an effectful computation
  buttons  : DrawEnv -> DrawState -> Act HTMLNodes

  ||| Make adjustments to the additional top bar buttons 
  adjust   : DrawEnv -> DrawEvent -> DrawState -> Act ()

--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

down : MouseInfo -> Maybe DrawEvent
down mi = case mi.button of
  0 => Just LeftDown
  1 => Just MiddleDown
  _ => Nothing

up : MouseInfo -> Maybe DrawEvent
up mi = case mi.button of
  0 => Just LeftUp
  1 => Just MiddleUp
  _ => Nothing

move : MouseInfo -> Maybe DrawEvent
move x = Just $ Move x.offsetX x.offsetY

wheel : WheelInfo -> Maybe DrawEvent
wheel wi =
  if wi.deltaY < 0 then Just (ZoomIn True)
     else if wi.deltaY > 0 then Just (ZoomOut True)
     else Nothing

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
moleculeCanvas : String -> Ref Div
moleculeCanvas pre = Id "\{pre}-molecule-canvas"

export
sketcherDiv : String -> Ref Div
sketcherDiv pre = Id "\{pre}-sketcher-div"

export
molReader : String -> Ref Div
molReader pre = Id "\{pre}-mol-reader"

export
molInput : String -> Ref TextArea
molInput pre = Id "\{pre}-mol-input"

export
leftBarID : String -> Ref Div
leftBarID pre = Id "\{pre}-left-bar"

export
rightBarID : String -> Ref Div
rightBarID pre = Id "\{pre}-right-bar"

export
topBarID : String -> Ref Div
topBarID pre = Id "\{pre}-top-bar"

export
bottomBarID : String -> Ref Div
bottomBarID pre = Id "\{pre}-bottom-bar"

export
abbrID : String -> Ref Tag.Select
abbrID pre = Id "\{pre}-abbreviations"

export
expButton : String -> Ref Tag.Button
expButton pre = Id "\{pre}-exp-button"

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------
  
hidden : {0 t : _} -> Attribute t
hidden = class "hidden"

abbrActive : DrawState -> Attribute t
abbrActive s =
  case s.mode of
    SetAbbr _ => active True
    _         => active False

drawing : MolBond -> DrawState -> Bool
drawing b s =
  (s.mode == Draw || s.mode == Drawing Nothing) &&
  (s.bond == b)

setting : Elem -> DrawState -> Bool
setting el s = s.mode == SetAtom (cast el)

%inline fromStereo : BondStereo -> MolBond
fromStereo = MkBond True Single

disable : Bool -> HTMLNode -> HTMLNode
disable b = withAttribute (disabled b)

minZoom : (s : DrawSettings) => AffineTransformation -> Bool
minZoom (AT tf _) = tf.scale <= s.minZoom

maxZoom : (s : DrawSettings) => AffineTransformation -> Bool
maxZoom (AT tf _) = tf.scale >= s.maxZoom

pse : Mode -> Bool
pse (PTable _)  = True
pse (SetAtom i) = all (i.elem /=) (the (List Elem) [C,O,N,F,P,S,Cl,Br])
pse _           = False

detail : String -> HTMLNode -> HTMLNode
detail title n =
  div
    [class "cyby-draw-detail"]
    [label [] [ Text title ], n]

px : Double -> String
px v = show (cast {to = Bits32} v) ++ "px"

parameters {auto de : Sink DrawEvent}

  elems : MolAtomAT -> HTMLNode
  elems a =
    selectFromListBy values (a.elem.elem ==) symbol ChgElem
      [ class widget, title "Set Element" ]

  charges : MolAtomAT -> HTMLNode
  charges a =
    selectFromListBy chs (a.charge ==) (show . value) ChgCharge
      [ class widget, title "Set Charge" ]
    where
      chs : List Charge
      chs = mapMaybe refineCharge [(-8) .. 8]

  massNrs : MolAtomAT -> HTMLNode
  massNrs a =
    selectFromListBy (masses a.elem.elem) (a.elem.mass ==) dispMass ChgMass
      [ class widget, title "Set Charge" ]
    where
      dispMass : Maybe MassNr -> String
      dispMass Nothing  = "Mix"
      dispMass (Just m) = show m.value
  
  icon :
       Classes
    -> DrawEvent
    -> (active : Bool)
    -> (title : String)
    -> HTMLNode
    -> HTMLNode
  icon cs ev a ttl child =
    button
      [classes (widget::icon::quadratic::cs),active a,onClick ev,title ttl]
      [child]

  abbrs : (ds : DrawSettings) => (pre : String) -> DrawState -> HTMLNode
  abbrs pre s =
    selectFromListBy
      ds.abbreviations
      (\a => any ((a.label ==) . label) s.abbr)
      label
      SelAbbr
      [ Id $ abbrID pre
      , class widget
      , abbrActive s
      , title "Abbreviations"
      , Event (MouseDown $ \mi => toMaybe (mi.button == 0) EnableAbbr)
      ]

  bondIcon : MolBond -> String -> DrawState -> HTMLNode -> HTMLNode
  bondIcon b title s = icon [] (SetBond b) (drawing b s) title

  topBar :
       {auto ds : DrawSettings}
    -> (pre     : String)
    -> (topadd  : HTMLNodes)
    -> DrawState
    -> HTMLNode
  topBar {ds} pre topadd s =
    div
      [ Id $ topBarID pre, class toolbarTop ] $
      [ icon [] SelectMode (s.mode == Select) "select" select
      , icon [] EraseMode (s.mode == Erase) "erase" erase
      , icon [] Clear False "clear" clear
      , disable (s.undos == []) $ icon [] Undo False "undo" undo
      , disable (s.redos == []) $ icon [] Redo False "redo" redo
      , icon [] Center False "center" center
      , disable (maxZoom s.transform) $ icon [] (ZoomIn False) False "zoom in" zoomIn
      , disable (minZoom s.transform) $ icon [] (ZoomOut False) False "zoom out" zoomOut
      , bondIcon (cast Single) "single bond" s single
      -- , bondIcon "single-up" (fromStereo Up) "single bond up" s
      -- , bondIcon "single-down" (fromStereo Down) "single bond down" s
      -- , bondIcon "single-up-down" (fromStereo Either) "single bond up or down" s
      , bondIcon (cast Types.Dbl) "double bond" s double
      , bondIcon (cast Triple) "triple bond" s triple
      ] ++ topadd

  template : (cls : Class) -> CDGraph -> String -> DrawState -> HTMLNode
  -- template cls g nm s =
  --   radioIcon cls (SetTempl g) "Template \{nm}" (s.mode == SetTempl g)

  elemIcon : DrawState -> String -> Elem -> HTMLNode
  elemIcon s t e = icon [elemText e] (SetElem e) (setting e s) t (Text $ symbol e)

  leftBar pre s =
    div
      [ Id $ leftBarID pre, class toolbarLeft ]
      [ elemIcon s "Boron" B
      , elemIcon s "Carbon" C
      , elemIcon s "Oxygen" O
      , elemIcon s "Nitrogen" N
      , elemIcon s "Fluorine" F
      , elemIcon s "Phosphorous" P
      , elemIcon s "Sulfur" S
      , elemIcon s "Chlorine" Cl
      , elemIcon s "Bromine" Br
      , icon [smallText] StartPSE (pse s.mode) "PSE" "PSE"
      ]

  rightBar : (pre : String) -> DrawState -> HTMLNode
  rightBar pre s =
    case selectedNodes s.imol False of
      [n] =>
        let atm     := atom $ lab s.imol n
            tpe     := atm.type.name
            [x,y,_] := atm.position
            cx      := dispCoordShort x
            cy      := dispCoordShort y
         in div
              [ Id $ rightBarID pre, class toolbarRight ]
              [ detail "Element"  $ elems atm
              , detail "Isotope"  $ massNrs atm
              , detail "Charge"   $ charges atm
              , detail "Type"     $ div [ class "cyby-draw-atomtype"] [Text tpe]
              , detail "x-Coord." $ div [ class "cyby-draw-atomtype"] [Text cx]
              , detail "y-Coord." $ div [ class "cyby-draw-atomtype"] [Text cy]
              ]
      _   => case selectedEdges s.imol of
        [(x,y)] =>
          let px := point $ position $ atom $ lab s.imol x
              py := point $ position $ atom $ lab s.imol y
              d  := printDouble 3 $ distance px py
              a  := angleOrZero (px - py)
              a' := printDouble (S Z) $ toDegree $ if a >= Angle.pi then (a - Angle.pi) else a
           in div
                [ Id $ rightBarID pre, class "cyby-draw-toolbar-right" ]
                [ detail "Length"   $ div [ class "cyby-draw-atomtype"] [Text "\{d} Å"]
                , detail "Angle"    $ div [ class "cyby-draw-atomtype"] [Text "\{a'}°"]
                ]
        _       => div [ Id $ rightBarID pre, class toolbarRight ] []

  bottomBar : (pre : String) -> DrawState -> HTMLNode
  bottomBar pre s =
    div
      [ Id $ bottomBarID pre, class "cyby-draw-toolbar-bottom-inner" ]
      []
      -- [ template "benzene" phenyl "Benzene" s
      -- , template "cyclohexane" (ring 6) "Cyclohexane" s
      -- , template "cyclopentane" (ring 5) "Cyclopentane" s
      -- , template "cyclopropane" (ring 3) "Cyclopropane" s
      -- , template "cyclobutane" (ring 4) "Cyclobutane" s
      -- , template "cycloheptane" (ring 7) "Cycloheptane" s
      -- , template "cyclooctane" (ring 8) "Cyclooctane" s
      -- ]

  export
  sketcher :
       {auto ds : DrawSettings}
    -> (pre     : String)
    -> (topadd  : HTMLNodes)
    -> DrawState
    -> HTMLNode
  sketcher pre topadd s =
    div
      [ class "cyby-draw-sketcher-div"
      , Id $ sketcherDiv pre
      ]
      [ topBar pre topadd s
      , leftBar pre s
      , rightBar pre s
      , div
          [ class "cyby-draw-molecule-canvas"
          , Id $ moleculeCanvas pre
          , Event $ MouseMove move
          , Event $ MouseDown down
          , Event $ MouseUp up
          , Event_ True False $ Wheel wheel
          , Event_ True False $ KeyDown (Just . KeyDown . key)
          , Event_ True False $ KeyUp (Just . KeyUp . key)
          , onMouseEnter Draw.Event.Focus
          , onMouseLeave Draw.Event.Blur
          , onDblClick Expand
          , onResize (\r => Resize r.height r.width)
          , Str "tabindex" "1"
          , style
              [ width $ px $ cast s.dims.swidth
              , height $ px $ cast s.dims.sheight
              ]
          ]
          [Raw s.curSVG]
      , div
          [ class toolbarBottom ]
          [ bottomBar pre s, abbrs pre s ]
      ]

export
cybyDrawBtn : Sink e => String -> e -> Attributes Tag.Button -> HTMLNode
cybyDrawBtn s e as = button (class widget :: onClick e :: as) [Text s]

export
expBtn : DrawEnv => String -> DrawState -> HTMLNode
expBtn @{DE pre} txt s =
  cybyDrawBtn txt SVG  [Id $ expButton pre, disabled $ emptyGraph s]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

molCanvasCls : Class
molCanvasCls = "cyby-draw-molecule-canvas"

parameters {auto ds : DrawSettings}
           {auto se : Sink DrawEvent}
           {auto lm : Loggable JS DrawMsg}
           {auto ex : Extension}
           (pre : String)

  canvasCls : List Class -> Act ()
  canvasCls = attr (moleculeCanvas pre) . classes . (molCanvasCls ::)

  rotating : Act ()
  rotating = canvasCls ["rotating"]

  dragging : Act ()
  dragging = canvasCls ["dragging"]

  normal : Act ()
  normal = canvasCls []

  selectCursor : DrawState -> Act ()
  selectCursor s =
    case s.mode of
      Dragging _    => dragging
      Rotating _    => rotating
      RotTempl _ _  => rotating
      Translating _ => dragging
      _             => applyWhenSel s dragging rotating normal

  adjAbbr : DrawState -> Act ()
  adjAbbr = attr (abbrID pre) . abbrActive

  focusCurrentApp : Act ()
  focusCurrentApp = focus (moleculeCanvas pre)

  displayST : (force : Bool) -> DrawState -> Act ()
  displayST force s =
    when (force || s.curSVG /= s.prevSVG) $
      child (moleculeCanvas pre) (Raw s.curSVG) >>
      when s.hasFocus focusCurrentApp

  adjustBars : DrawState -> Act ()
  adjustBars s = Prelude.do
    topadd <- ex.buttons (DE pre) s
    replace (topBarID pre) (topBar pre topadd s)
    replace (bottomBarID pre) (bottomBar pre s)
    replace (leftBarID pre) (leftBar pre s)
    adjAbbr s

  adjustRightBar : DrawState -> Act ()
  adjustRightBar s = do
    replace (rightBarID pre) (rightBar pre s)
    adjAbbr s

  dispKeyDown : String -> DrawState -> Act ()
  dispKeyDown "Escape" s = Prelude.do
    topadd <- ex.buttons (DE pre) s
    replace (sketcherDiv pre) (sketcher pre topadd s)
  dispKeyDown "c" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph True s.mol
       in when (g.order > 0) (molToClipboard g >> logLoggable Copied)
  dispKeyDown "x" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph False s.mol
       in when (g.order > 0) (molToClipboard g >> logLoggable Copied)
  dispKeyDown "v"    s =
    -- we need to read from the clipboard in a new fiber, because the result
    -- will be written to the sink of `DrawEvent`s, which we are currently
    -- processing
    when (s.modifier == Ctrl) (ignore $ start fromClipboard)
  dispKeyDown "Ctrl" s = selectCursor s
  dispKeyDown _      s = pure ()

  displayEv : DrawEvent -> DrawState -> Act ()
  displayEv Focus            s = focusCurrentApp
  displayEv Blur             s = blur (moleculeCanvas pre)
  displayEv (KeyDown k)      s = dispKeyDown k s
  displayEv (KeyUp _)        s = adjustRightBar s
  displayEv (SetElem _)      s = adjustBars s
  displayEv (SelAbbr _)      s = adjustBars s
  displayEv  EnableAbbr      s = adjustBars s
  displayEv (SetBond _)      s = adjustBars s
  displayEv (SetTempl _)     s = adjustBars s
  displayEv (Load _)         s = adjustBars s
  displayEv SelectMode       s = adjustBars s
  displayEv EraseMode        s = adjustBars s
  displayEv (ChgElem _)      s = adjustRightBar s
  displayEv (Move _ _)       s = selectCursor s
  displayEv MiddleDown       s = selectCursor s
  displayEv MiddleUp         s = selectCursor s
  displayEv LeftUp           s = adjustBars s >> adjustRightBar s
  displayEv Undo             s = adjustBars s
  displayEv Redo             s = adjustBars s
  displayEv (ZoomIn _)       s = adjustBars s
  displayEv (ZoomOut _)      s = adjustBars s
  displayEv Clear            s = adjustBars s
  displayEv SVG              s = ex.doExport s
  displayEv Redraw           s = displayST True s
  displayEv _                s = pure ()


  export
  displaySketcher : DrawEvent -> DrawState -> Act ()
  displaySketcher e s =
    displayEv e s >> displayST False s >> ex.adjust (DE pre) e s

export
disableExport : DrawEnv => DrawState -> Act ()
disableExport @{DE pre} = disabled (expButton pre) . emptyGraph

||| Renders a molecule at the given canvas.
|||
||| The molecule will be scaled and centered to fit the canvas and
||| the given nodes will be highlighted.
export
displayMol :
     {auto ds : DrawSettings}
  -> SceneDims
  -> MolGraphAT
  -> Maybe (List Nat)
  -> HTMLNode
displayMol sd g m =
  let cdg    := initGraph g
      G o mg := maybe cdg (\ns => highlight ns cdg) m
   in Raw . curSVG $ initMol sd Fill False "" $ G o mg

||| An editor for molecules.
export
molEdit :
     {auto ex : Extension}
  -> {auto lg : Loggable JS DrawMsg}
  -> Act DrawSettings
  -> SceneDims
  -> Editor MolfileAT
molEdit getDS sd =
  E $ \m => Prelude.do
   ui     <- map interpolate uniqueID
   ds     <- getDS
   E es   <- event DrawEvent
   let st := fromMol sd Init (maybe (G 0 empty) graph m)
   topadd <- ex.buttons (DE ui) st
   let nd := sketcher ui topadd st
   pure $ Widget.W nd $
     es |> P.evalScans1 st (doact ui)
        |> (\x => cons st x)
        |> P.mapOutput (Valid . toMolfile . mol)

   where
     doact :
          {auto ds : DrawSettings}
       -> {auto se : Sink DrawEvent}
       -> (pre     : String)
       -> DrawState
       -> DrawEvent
       -> Act DrawState
     doact pre s e = let s2 := update e s in displaySketcher pre e s2 $> s2

||| The default `Extension`
export %hint
NoExt : Extension
NoExt =
  E
    { doExport = storeSVG . exportSVG
    , buttons  = \_,s => pure [expBtn "Save..." s]
    , adjust   = \_,_,s => disableExport s
    }
