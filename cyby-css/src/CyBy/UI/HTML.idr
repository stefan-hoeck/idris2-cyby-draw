module CyBy.UI.HTML

import Data.List
import IO.Async.Logging

import public CyBy.UI.CSS.Classes
import public Text.HTML
import public Text.HTML.DomID

%default total

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

export
formSep : HTMLNode
formSep = div [class formsep] []

export
hbarSep : HTMLNode
hbarSep = div [class hbarsep] []

export
vbarSep : HTMLNode
vbarSep = div [class vbarsep] []

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

export
CyByLog : DomID
CyByLog = "cyby-log"

export
logNode : LogLevel -> List String -> HTMLNode
logNode l msgs =
  li [class listEntry]
    [ div [class $ level l] [Text $ "[\{l}]"]
    , div [class listEntryValue] $ intersperse (br []) (map Text msgs)
    ]

export
appLog : HTMLNode
appLog = div [class drawLog] [h1 [] ["Log"], ul [ref CyByLog] []]

--------------------------------------------------------------------------------
-- Icons
--------------------------------------------------------------------------------

export
pathNoStroke : String
pathNoStroke = "path style='stroke:none;fill:currentcolor;'"

-- The following icons are to be inlined directly in the DOM. We use
-- `currentcolor` to adopt the coloring of the parent container.

||| Icon for selecting items in the UI
export
select : HTMLNode
select = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 3.332031 16.667969 L 3.332031 17.5 L 1.667969 17.5 L 1.667969 15.832031 L 2.5 15.832031 L 2.5 16.667969 Z M 2.5 2.5 L 3.332031 2.5 L 3.332031 1.667969 L 1.667969 1.667969 L 1.667969 3.332031 L 2.5 3.332031 Z M 1.667969 6.667969 L 2.5 6.667969 L 2.5 5 L 1.667969 5 Z M 1.667969 10.832031 L 2.5 10.832031 L 2.5 8.332031 L 1.667969 8.332031 Z M 16.667969 6.667969 L 17.5 6.667969 L 17.5 5 L 16.667969 5 Z M 16.667969 10 L 17.5 10 L 17.5 8.332031 L 16.667969 8.332031 Z M 1.667969 14.167969 L 2.5 14.167969 L 2.5 12.5 L 1.667969 12.5 Z M 6.667969 2.5 L 6.667969 1.667969 L 5 1.667969 L 5 2.5 Z M 10.832031 2.5 L 10.832031 1.667969 L 8.332031 1.667969 L 8.332031 2.5 Z M 7.5 17.5 L 7.5 16.667969 L 5.832031 16.667969 L 5.832031 17.5 Z M 10.832031 17.5 L 10.832031 16.667969 L 9.167969 16.667969 L 9.167969 17.5 Z M 14.167969 2.5 L 14.167969 1.667969 L 12.5 1.667969 L 12.5 2.5 Z M 15.832031 1.667969 L 15.832031 2.5 L 16.667969 2.5 L 16.667969 3.332031 L 17.5 3.332031 L 17.5 1.667969 Z M 15.628906 18.332031 L 17.867188 17.207031 L 15.9375 13.332031 L 19.167969 13.332031 L 11.667969 7.886719 L 11.667969 17.136719 L 13.652344 14.417969 Z M 13.78125 12.824219 L 12.5 14.582031 L 12.5 9.523438 L 16.601562 12.5 L 14.59375 12.5 L 16.75 16.835938 L 15.996094 17.214844 Z M 13.78125 12.824219'/></svg>"

||| Icon of an eraser
export
erase : HTMLNode
erase = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 10.109375 2.757812 C 11.082031 1.78125 12.667969 1.78125 13.640625 2.757812 L 18.492188 7.609375 C 19.46875 8.582031 19.46875 10.167969 18.492188 11.140625 L 11.617188 18.015625 C 11.148438 18.488281 10.511719 18.75 9.847656 18.75 L 6.398438 18.75 C 5.738281 18.75 5.101562 18.488281 4.632812 18.015625 L 1.507812 14.890625 C 0.53125 13.917969 0.53125 12.332031 1.507812 11.359375 L 10.105469 2.757812 Z M 12.757812 3.640625 C 12.269531 3.15625 11.480469 3.15625 10.992188 3.640625 L 5.199219 9.433594 L 11.816406 16.050781 L 17.609375 10.257812 C 18.09375 9.769531 18.09375 8.980469 17.609375 8.492188 Z M 10.933594 16.933594 L 4.316406 10.316406 L 2.390625 12.242188 C 1.90625 12.730469 1.90625 13.519531 2.390625 14.007812 L 5.515625 17.132812 C 5.75 17.367188 6.070312 17.5 6.402344 17.5 L 9.851562 17.5 C 10.179688 17.5 10.5 17.367188 10.734375 17.132812 Z M 10.933594 16.933594'/></svg>"

||| Icon of a trash can
export
trash : HTMLNode
trash = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 15.726562 5 L 16.5625 5 L 15.742188 18.347656 C 15.710938 18.808594 15.328125 19.167969 14.867188 19.167969 L 5.132812 19.167969 C 4.671875 19.164062 4.289062 18.808594 4.257812 18.347656 L 3.4375 5 L 4.273438 5 L 5.089844 18.292969 C 5.089844 18.316406 5.109375 18.332031 5.132812 18.332031 L 14.867188 18.332031 Z M 7.917969 15.832031 C 8.148438 15.832031 8.332031 15.648438 8.332031 15.417969 L 8.332031 7.082031 C 8.332031 6.851562 8.148438 6.667969 7.917969 6.667969 C 7.6875 6.667969 7.5 6.851562 7.5 7.082031 L 7.5 15.417969 C 7.5 15.648438 7.6875 15.832031 7.917969 15.832031 Z M 12.082031 15.832031 C 12.3125 15.832031 12.5 15.648438 12.5 15.417969 L 12.5 7.082031 C 12.5 6.851562 12.3125 6.667969 12.082031 6.667969 C 11.851562 6.667969 11.667969 6.851562 11.667969 7.082031 L 11.667969 15.417969 C 11.667969 15.648438 11.851562 15.832031 12.082031 15.832031 Z M 4.21875 4.167969 L 2.5 4.167969 L 2.5 3.332031 L 6.667969 3.332031 L 6.667969 2.707031 C 6.667969 2.132812 7.132812 1.667969 7.707031 1.667969 L 12.292969 1.667969 C 12.867188 1.667969 13.332031 2.132812 13.332031 2.707031 L 13.332031 3.332031 L 17.5 3.332031 L 17.5 4.167969 Z M 7.5 3.332031 L 12.5 3.332031 L 12.5 2.707031 C 12.5 2.59375 12.40625 2.5 12.292969 2.5 L 7.707031 2.5 C 7.59375 2.5 7.5 2.59375 7.5 2.707031 Z M 7.5 3.332031'/></svg>"

||| Undo icon (counter clockwise arrow)
export
undo : HTMLNode
undo = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 6.667969 5.417969 L 6.667969 7.6875 C 7.5 6.800781 8.6875 6.25 10 6.25 C 12.53125 6.25 14.582031 8.300781 14.582031 10.832031 C 14.582031 12.097656 14.070312 13.246094 13.242188 14.074219 L 12.210938 13.042969 C 12.773438 12.476562 13.125 11.695312 13.125 10.832031 C 13.125 9.105469 11.726562 7.707031 10 7.707031 C 9.074219 7.707031 8.242188 8.109375 7.671875 8.75 L 10 8.75 L 8.75 10 L 5.417969 10 L 5.417969 6.667969 Z M 6.667969 5.417969 '/></svg>"

||| Redo icon (clockwise arrow)
export
redo : HTMLNode
redo = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 13.332031 5.417969 L 14.582031 6.667969 L 14.582031 10 L 11.25 10 L 10 8.75 L 12.328125 8.75 C 11.757812 8.109375 10.925781 7.707031 10 7.707031 C 8.273438 7.707031 6.875 9.105469 6.875 10.832031 C 6.875 11.695312 7.226562 12.476562 7.789062 13.042969 L 6.757812 14.074219 C 5.929688 13.246094 5.417969 12.097656 5.417969 10.832031 C 5.417969 8.300781 7.46875 6.25 10 6.25 C 11.3125 6.25 12.496094 6.800781 13.332031 7.6875 Z M 13.332031 5.417969 '/></svg>"

||| Icon for centering a molecule
export
center : HTMLNode
center = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 5 1.25 L 1.25 1.25 L 1.25 5 L 2.5 5 L 2.5 2.5 L 5 2.5 Z M 5 1.25 '/><\{pathNoStroke} d='M 15 1.25 L 18.75 1.25 L 18.75 5 L 17.5 5 L 17.5 2.5 L 15 2.5 Z M 15 1.25 '/><\{pathNoStroke} d='M 5 18.75 L 1.25 18.75 L 1.25 15 L 2.5 15 L 2.5 17.5 L 5 17.5 Z M 5 18.75 '/><\{pathNoStroke} d='M 15 18.75 L 18.75 18.75 L 18.75 15 L 17.5 15 L 17.5 17.5 L 15 17.5 Z M 15 18.75 '/><\{pathNoStroke} d='M 15 15 L 5 15 C 4.308594 15 3.75 14.441406 3.75 13.75 L 3.75 6.25 C 3.75 5.558594 4.308594 5 5 5 L 15 5 C 15.691406 5 16.25 5.558594 16.25 6.25 L 16.25 13.75 C 16.25 14.441406 15.691406 15 15 15 Z M 5 6.25 L 5 13.75 L 15 13.75 L 15 6.25 Z M 5 6.25 '/></svg>"

||| Lens with a plus sign
export
zoomIn : HTMLNode
zoomIn = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 18.96875 17.0625 L 15.4375 13.53125 C 15.171875 13.265625 14.746094 13.265625 14.484375 13.53125 L 14.300781 13.710938 L 13.082031 12.492188 L 13.242188 12.332031 C 15.601562 9.484375 15.304688 5.289062 12.574219 2.800781 C 9.839844 0.308594 5.636719 0.410156 3.023438 3.023438 C 0.410156 5.636719 0.308594 9.839844 2.800781 12.574219 C 5.289062 15.304688 9.484375 15.601562 12.332031 13.242188 L 12.492188 13.082031 L 13.710938 14.300781 L 13.53125 14.484375 C 13.265625 14.746094 13.265625 15.175781 13.53125 15.4375 L 17.0625 18.96875 C 17.328125 19.230469 17.753906 19.230469 18.015625 18.96875 L 18.96875 18.015625 C 19.230469 17.753906 19.230469 17.328125 18.96875 17.0625 Z M 7.914062 14 C 6.300781 14.003906 4.753906 13.363281 3.617188 12.21875 C 1.292969 9.898438 1.230469 6.148438 3.480469 3.75 C 5.730469 1.355469 9.476562 1.179688 11.941406 3.355469 C 14.40625 5.53125 14.695312 9.269531 12.597656 11.796875 L 11.742188 12.648438 C 10.660156 13.527344 9.308594 14.003906 7.914062 14 Z M 17.539062 18.265625 L 14.230469 14.960938 L 14.960938 14.234375 L 18.269531 17.539062 Z M 8.332031 7.5 L 10.832031 7.5 L 10.832031 8.332031 L 8.332031 8.332031 L 8.332031 10.832031 L 7.5 10.832031 L 7.5 8.332031 L 5 8.332031 L 5 7.5 L 7.5 7.5 L 7.5 5 L 8.332031 5 Z M 8.332031 7.5 '/></svg>"

||| Lens with a minus sign
export
zoomOut : HTMLNode
zoomOut = Raw "<svg viewBox='0 0 20 20'><\{pathNoStroke} d='M 18.96875 17.0625 L 15.4375 13.53125 C 15.171875 13.265625 14.746094 13.265625 14.484375 13.53125 L 14.300781 13.710938 L 13.082031 12.492188 L 13.242188 12.332031 C 15.601562 9.484375 15.304688 5.289062 12.574219 2.800781 C 9.839844 0.308594 5.636719 0.410156 3.023438 3.023438 C 0.410156 5.636719 0.308594 9.839844 2.800781 12.574219 C 5.289062 15.304688 9.484375 15.601562 12.332031 13.242188 L 12.492188 13.082031 L 13.710938 14.300781 L 13.53125 14.484375 C 13.265625 14.746094 13.265625 15.175781 13.53125 15.4375 L 17.0625 18.96875 C 17.328125 19.230469 17.753906 19.230469 18.015625 18.96875 L 18.96875 18.015625 C 19.230469 17.753906 19.230469 17.328125 18.96875 17.0625 Z M 7.914062 14 C 6.300781 14.003906 4.753906 13.363281 3.617188 12.21875 C 1.292969 9.898438 1.230469 6.148438 3.480469 3.75 C 5.730469 1.355469 9.476562 1.179688 11.941406 3.355469 C 14.40625 5.53125 14.695312 9.269531 12.597656 11.796875 L 11.742188 12.648438 C 10.660156 13.527344 9.308594 14.003906 7.914062 14 Z M 17.539062 18.265625 L 14.230469 14.960938 L 14.960938 14.234375 L 18.269531 17.539062 Z M 5 7.5 L 10.832031 7.5 L 10.832031 8.332031 L 5 8.332031 Z M 5 7.5 '/></svg>"

||| A single bond (TODO: fix its size)
export
single : HTMLNode
single = Raw "<svg viewBox='0 0 20 20'><path style='fill:none;stroke:currentcolor;stroke-width:1;stroke-linecap:round' d='M 17.134464,3.1507937 2.9920635,17.291594'/></svg>"

||| A double bond
export
double : HTMLNode
double = Raw "<svg viewBox='0 0 20 20'><g transform='translate(-32.888486,-15.32218)'><path style='fill:none;stroke:currentcolor;stroke-width:1;stroke-linecap:round' d='m 37.34373,33.95029 14.1424,-14.1408'/><path style='fill:none;stroke:currentcolor;stroke-width:1;stroke-linecap:round' d='m 34.51547,31.12171 14.1424,-14.1408'/></g></svg>"
||| A triple bond

export
triple : HTMLNode
triple = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.90059434,0,0,0.90059387,-56.99508,-11.508089)' style='stroke-width:1'><path style='fill:none;stroke:currentcolor;stroke-width:1.11038;stroke-linecap:round' d='M 67.392,30.9552 81.536,16.8144'/><path style='fill:none;stroke:currentcolor;stroke-width:1.11038;stroke-linecap:round' d='m 70.22011,33.78395 14.144,-14.1408'/><path style='fill:none;stroke:currentcolor;stroke-width:1.11038;stroke-linecap:round' d='m 64.56389,28.12645 14.144,-14.1408'/></g></svg>"

||| A wedged bond towards the viewer
export
bondUp : HTMLNode
bondUp = Raw "<svg viewBox='0 0 20 20'><g style='fill:currentcolor;currentcolor;stroke-width:1;stroke:currentcolor;stroke-linecap:round;stroke-linejoin:round' transform='translate(406.95379,162.55406)'><polygon points='-404.96175,-144.54576 -392.54997,-160.41917 -389.01483,-156.88323 -404.89105,-144.47504'</g></svg>"

||| A wedged bond away from the viewer
export
bondDown : HTMLNode
bondDown = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(1.0889093,0,0,1.0889103,-148.48125,-12.18109)' style='stroke-width:1'><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 152.86559,16.42123 -3.04022,-3.0409'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 150.49657,18.2948 -2.5453,-2.54588'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 148.12755,20.16836 -2.05038,-2.05084'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 145.75853,22.04193 -1.55546,-1.55582'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 143.38951,23.91549 -1.06054,-1.06078'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 141.02049,25.78906 -0.56562,-0.56575'/><path style='fill:none;stroke:currentcolor;stroke-width:0.91835;stroke-linecap:round' d='m 138.65147,27.6626 -0.0707,-0.0707'/></g></svg>"

||| A wavy bond
export
bondEither : HTMLNode
bondEither = Raw "<svg viewBox='0 0 20 20'><path style='fill:none;stroke:currentcolor;stroke-width:1;stroke-linecap:round' d='M 2.3254458,17.784193 A 1.607724,1.6077546 0 0 1 4.2202771,15.88954 1.607724,1.6077546 0 0 0 6.1151085,13.994887 1.607724,1.6077546 0 0 1 8.0099398,12.100234 1.607724,1.6077546 0 0 0 9.9047604,10.205591 1.607724,1.6077546 0 0 1 11.799592,8.3109377 1.607724,1.6077546 0 0 0 13.694423,6.4162846 1.607724,1.6077546 0 0 1 15.589244,4.5216423 1.607724,1.6077546 0 0 0 17.484075,2.6269893'/></svg>"

export
cyclopropane : HTMLNode
cyclopropane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.68754514,0,0,0.68564061,142.00812,87.13402)' style='fill:none;stroke:currentcolor;stroke-width:0.971469;stroke-linecap:round'><path d='m -182,-103.84 -10,-17.3184 m -9.9984,17.3184 9.9984,-17.3184 m -9.9984,17.3184 H -182'/></g></svg>"

export
cyclobutane : HTMLNode
cyclobutane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.624986,0,0,0.624986,132.49726,78.435467)' style='fill:none;stroke:currentcolor;stroke-width:1.06722;stroke-linecap:round'><path d='m -206,-99.4992 h 20 m -0.002,-20 0.002,20 m -20,-20 v 20 m 19.9984,-20 H -206'/></g></svg>"

export
cyclopentane : HTMLNode
cyclopentane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.44796566,0,0,0.44739288,108.10448,58.094377)' style='fill:none;stroke:currentcolor;stroke-width:1.48991;stroke-linecap:round'><path d='M -235.1792,-103.8672 -219,-92.1104 m 16.1792,-11.7568 L -219,-92.1104 m -10,-30.7776 -6.1792,19.0208 m 26.1776,-19.0208 H -229 m 26.1792,19.0208 -6.1808,-19.0208'/></g></svg>"

export
cyclohexane : HTMLNode
cyclohexane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.45314325,0,0,0.45366675,102.89437,76.915573)' style='fill:none;stroke:currentcolor;stroke-width:1.47109;stroke-linecap:round'><path d='m -205,-127.4992 17.32,-10 m 0,-20 v 20 m -34.64,0 17.32,10 m -17.32,-29.9984 v 19.9984 m 17.32,-30 -17.32,10.0016 m 34.64,-0.002 -17.32,-10'/></g></svg>"

export
cycloheptane : HTMLNode
cycloheptane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.40637446,0,0,0.40615158,98.183258,63.815084)' style='fill:none;stroke:currentcolor;stroke-width:1.64179;stroke-linecap:round'><path d='m -235.0192,-119.2704 18.0192,8.6768 m 18.0208,-8.6768 -18.0208,8.6768 m -22.4688,-28.176 4.4496,19.4992 m 8.0192,-35.136 -12.4688,15.6368 M -207,-154.4064 h -20 m 32.4688,15.6368 L -207,-154.4064 m 8.0208,35.136 4.448,-19.4992'/></g></svg>"

export
cyclooctane : HTMLNode
cyclooctane = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.36164455,0,0,0.3616441,71.479284,74.191249)' style='fill:none;stroke:currentcolor;stroke-width:1.84435;stroke-linecap:round'><path d='m -160,-153.3584 14.1424,-14.1408 m 0,-20 v 20 M -180,-153.3584 h 20 m -34.1408,-14.1408 14.1408,14.1408 m -14.1408,-34.1392 v 19.9984 m 14.1408,-34.1392 -14.1408,14.1408 M -160,-201.6384 h -20 m 34.1424,14.1392 L -160,-201.6384'/></g></svg>"

export
benzene : HTMLNode
benzene = Raw "<svg viewBox='0 0 20 20'><g transform='matrix(0.45096964,0,0,0.45303572,116.42884,56.435799)' style='fill:none;stroke:currentcolor;stroke-width:1.47492141;stroke-linecap:round'><path d='m -236,-122.4992 -17.3184,10 m 17.31816,-5.3812 13.31852,7.69039 M -236,-122.4992 l 17.3184,10 m -30.6368,2.3094 v 15.381199 m -4,-17.690599 v 20 m 0,0 17.3184,10 m -2.4e-4,-4.618802 13.31852,-7.690388 M -236,-82.4992 l 17.3184,-10 m 0,0 v -20'/></g></svg>"

||| base-64 encoded png of the rotate icon
export
rotate : String
rotate = "iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRTb2Z0d2FyZQB3d3cuaW5rc2NhcGUub3Jnm+48GgAAATlJREFUOI2t1L8rt1EYx/HXhcQgkygZDKRkYkOZlNm/oWzKIFam58litTH4JwzKbPEjZZNsFE89OIbvwZfur/u+5arTqXN9rvc5netHpJT8prVVEUVE568CcRwRw5WUKaWWC71YxAtuMfedPqVUDEQP/uABqWnt1gZiCOe4xwbG8B9riFpAdOMEVxhtOl8oA7UCruEfJqoCWgJzxq+x81NYSulT2YxjAAcVS6nQmoGDeb+sGhwRXRFxGBFjRcDHvHfXeNAIZtFXBDzXqLXJGsDJHHPxdhDNwyEijvCSUpopI0VE4E0//e74UjYL+cal0vJgOWvnyzplG09YQUeBvwOreMbfKq3Xji2NgXCKdY0BsajRimfZt4n2SsMhg6ewjzsfw+EOe5hqFfcpKd8koD//902ptgqwjr0CIOjbf9SaOuwAAAAASUVORK5CYII="
