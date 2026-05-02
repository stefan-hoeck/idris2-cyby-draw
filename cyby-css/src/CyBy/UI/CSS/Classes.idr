module CyBy.UI.CSS.Classes

import Chem.Elem
import Derive.Prelude
import Data.String
import IO.Async.Logging
import Text.HTML.Attribute
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
-- Utility Classes
--------------------------------------------------------------------------------

export %inline
sep : Class
sep = "cyby-sep"

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
moleculeCanvas : Class
moleculeCanvas = "cyby-draw-molecule-canvas"

export %inline
listEntry : Class
listEntry = "cyby-list-entry"

export %inline
listEntryValue : Class
listEntryValue = "cyby-list-entry-value"

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
