||| This module takes care of all events used for the word add-in (extension)
module CyBy.Draw.Extensions.Word

import Web.Async.Util

import CyBy.Draw.Event
import CyBy.Draw.MoleculeCanvas
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Extensions.Util
import Text.Molfile

%default total

-- uses the Word-API for exporting an svg string to a word document
-- image (inlinePicture)
%foreign
  """
  browser:lambda:(s,w) => {
    Word.run(async (context) => {
      const b64 = btoa(s);
      const docSelection = context.document.getSelection();
      docSelection.insertInlinePictureFromBase64(b64, Word.InsertLocation.end);
      context.sync();
    });
  }
  """
prim__exportImageToWord : String -> PrimIO ()

-- uses the Word-API for extracting an svg string from a word document image
-- async / await is needed for promises, see
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
-- for further informations
%foreign
  """
  browser:lambda:(f,w) => {
    Word.run(async context => {
      const selection = context.document.getSelection();
      selection.load('inlinePictures');
      await context.sync();
      const items = selection.inlinePictures.items;

      if (items.length > 0) {
        const b64 = items[0].getBase64ImageSrc();
        await context.sync();
        const valB64 = b64.value;
        context.sync();
        f(atob(valB64))(w);
      } else f('')(w);
    });
  }
  """
prim__importImageFromWord : (String -> PrimIO ()) -> PrimIO ()

%inline
exportImageToWord : HasIO io => String -> io ()
exportImageToWord s = primIO $ prim__exportImageToWord s

parameters {auto ds : DrawSettings}
           {auto se : Sink DrawEvent}
           {auto sm : Sink DrawMsg}

  fromClipboard : HasIO io => io ()
  fromClipboard =
    primIO $ prim__importImageFromWord $ \s =>
      toPrim $ case extractMetadata s >>= readMolfileE of
        Left s  => sink (ReadErr s)
        Right g => sink (Event.SetTempl g)

  ||| Parses a word event and forms a DrawEvent command.
  export
  dispWordExt : ExtensionEvent -> DrawState -> Act ()
  dispWordExt ExportSVG s = exportImageToWord $ exportSVG s
  dispWordExt ImportSVG s = fromClipboard
