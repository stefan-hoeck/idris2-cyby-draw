module CyBy.UI.CSS.Classes

import Chem.Elem
import Derive.Prelude
import Text.HTML.Attribute
import Text.HTML.DomID
import public Text.HTML.Extra.Class

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
elem : Class
elem = "cyby-draw-elem"

export %inline
pseIcon : Class
pseIcon = "cyby-draw-pse-icon"

export %inline
moleculeCanvas : Class
moleculeCanvas = "cyby-draw-molecule-canvas"
