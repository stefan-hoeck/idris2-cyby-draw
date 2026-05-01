module CyBy.UI.CSS.Classes

import Chem.Elem
import Data.String
import IO.Async.Logging
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
quadratic : Class
quadratic = "cyby-quadratic"

export %inline
smallText : Class
smallText = "cyby-small-text"

export %inline
formsep : Class
formsep = "cyby-form-sep"

export %inline
hbarsep : Class
hbarsep = "cyby-hbar-sep"

export %inline
vbarsep : Class
vbarsep = "cyby-vbar-sep"

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
drawDetails : Class
drawDetails = "cyby-draw-details"

export %inline
drawLog : Class
drawLog = "cyby-draw-log"

export %inline
compList : Class
compList = "cyby-comp-list"

export %inline
compTitle : Class
compTitle = "cyby-comp-title"

export %inline
moleculeCanvas : Class
moleculeCanvas = "cyby-draw-molecule-canvas"

export %inline
rotating : Class
rotating = "cyby-draw-rotating"

export %inline
dragging : Class
dragging = "cyby-draw-dragging"

export %inline
formRow : Class
formRow = "cyby-form-row"

export %inline
formLabel : Class
formLabel = "cyby-form-label"

export %inline
formValue : Class
formValue = "cyby-form-value"

export
level : LogLevel -> Class
level l = C "cyby-loglvl-\{l}"

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
