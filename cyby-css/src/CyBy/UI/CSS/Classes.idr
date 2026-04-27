module CyBy.UI.CSS.Classes

import Chem.Elem
import Data.String
import Text.HTML.Attribute
import public Text.CSS.Class

%default total

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

export %inline
active : Bool -> Attribute t
active = Bool "data-active"

export %inline
invalid : Bool -> Attribute t
invalid = Bool "data-invalid"

--------------------------------------------------------------------------------
-- Utility Classes
--------------------------------------------------------------------------------

export %inline
hidden : Class
hidden = "hidden"

export %inline
quadratic : Class
quadratic = "cyby-quadratic"

export %inline
smallText : Class
smallText = "cyby-small-text"

--------------------------------------------------------------------------------
-- Components
--------------------------------------------------------------------------------

export %inline
sketcherDiv : Class
sketcherDiv = "cyby-draw-sketcher-div"

export %inline
toolbarTop : Class
toolbarTop = "cyby-draw-toolbar-top"

export %inline
toolbarLeft : Class
toolbarLeft = "cyby-draw-toolbar-left"

export %inline
toolbarRight : Class
toolbarRight = "cyby-draw-toolbar-right"

export %inline
toolbarBottom : Class
toolbarBottom = "cyby-draw-toolbar-bottom"

export %inline
moleculeCanvas : Class
moleculeCanvas = "cyby-draw-molecule-canvas"

export %inline
rotating : Class
rotating = "cyby-draw-rotating"

export %inline
dragging : Class
dragging = "cyby-draw-dragging"

--------------------------------------------------------------------------------
-- Icons
--------------------------------------------------------------------------------

export %inline
fillPath : Class
fillPath = "cyby-fill-path"

export %inline
molPath : Class
molPath = "cyby-mol-path"

export %inline
molFillPath : Class
molFillPath = "cyby-molfill-path"

--------------------------------------------------------------------------------
-- Interactive Elements
--------------------------------------------------------------------------------

export %inline
widget : Class
widget = "cyby-widget"

||| A button with an background image an therefore no padding
export %inline
icon : Class
icon = "cyby-icon"

export %inline
elemText : Elem -> Class
elemText el = C "cyby-elem-text-\{toLower $ symbol el}"
