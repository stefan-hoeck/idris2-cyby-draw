module CyBy.UI.CSS.Classes

import Chem.Elem
import Derive.Prelude
import Data.String
import IO.Async.Logging
import Text.HTML.Attribute
import Text.HTML.DomID
import public Text.CSS.Class

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

public export
data DragMode = None | Rotating | Dragging

%runElab derive "DragMode" [Show,Eq]

export %inline
dragMode : DragMode -> Attribute t
dragMode = Str "data-dragmode" . toLower . show

export %inline
active : Bool -> Attribute t
active = Bool "data-active"

export %inline
invalid : Bool -> Attribute t
invalid = Bool "data-invalid"

--------------------------------------------------------------------------------
-- IDs
--------------------------------------------------------------------------------

export
CyByLog : DomID
CyByLog = "cyby-log"

--------------------------------------------------------------------------------
-- Utility Classes
--------------------------------------------------------------------------------

export %inline
sep : Class
sep = "cyby-sep"

export %inline
spacer : Class
spacer = "cyby-spacer"

--------------------------------------------------------------------------------
-- Components
--------------------------------------------------------------------------------

export %inline
sketcher : Class
sketcher = "cyby-draw-sketcher"

export %inline
drawUtils : Class
drawUtils = "cyby-draw-utils"

export %inline
drawElems : Class
drawElems = "cyby-draw-elems"

export %inline
drawInfo : Class
drawInfo = "cyby-draw-info"

export %inline
drawTemplates : Class
drawTemplates = "cyby-draw-templates"

export %inline
drawDetails : Class
drawDetails = "cyby-draw-details"

export %inline
drawLog : Class
drawLog = "cyby-draw-log"

export %inline
elem : Class
elem = "cyby-draw-elem"

export %inline
pseIcon : Class
pseIcon = "cyby-draw-pse-icon"

export %inline
moleculeCanvas : Class
moleculeCanvas = "cyby-draw-molecule-canvas"

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

||| A round icon
export %inline
roundIcon : Class
roundIcon = "cyby-round-icon"

export %inline
validatedInput : Class
validatedInput = "cyby-validated-input"

export %inline
iconPlaceholder : Class
iconPlaceholder = "cyby-icon-placeholder"

export %inline
iconMissing : Class
iconMissing = "cyby-icon-missing"

export %inline
iconError : Class
iconError = "cyby-icon-error"

export %inline
expandIcon : Class
expandIcon = "cyby-expand-icon"

export %inline
deleteIcon : Class
deleteIcon = "cyby-delete-icon"

export %inline
okIcon : Class
okIcon = "cyby-ok-icon"

export %inline
addIcon : Class
addIcon = "cyby-add-icon"

export %inline
trueIcon : Class
trueIcon = "cyby-true-icon"

export %inline
falseIcon : Class
falseIcon = "cyby-false-icon"
