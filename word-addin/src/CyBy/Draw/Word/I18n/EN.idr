module CyBy.Draw.Word.I18n.EN

import CyBy.Draw.I18n.EN
import public CyBy.Draw.Word.I18n

%default total

parameters {auto log : Logger JS}

  export
  WordLocal where
    emptyMol          = Caught "no or empty molecule"
    emptySel          = Caught "no or empty selection"
    emptySelection    = debug "no selection or no SVG image found: inserting new image"
    fromWord          = "from Word"
    imageReplaced     = debug "image replaced"
    insertingImage    = debug "inserting image"
    loadErr s         = Caught "error when reading molecule: \{s}"
    loadingImage      = debug "loading image from Word"
    logDimensions w h = debug "new width: \{w}; new height: \{h}"
    molLoaded         = debug "molecule loaded"
    newInserted       = debug "new image inserted"
    noMolfile         = Caught "no .mol data found in selected image"
    toWord            = "to Word"
