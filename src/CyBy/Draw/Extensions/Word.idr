||| This module takes care of all events used for the word add-in (extension)
module CyBy.Draw.Extensions.Word

import Web.MVC

import CyBy.Draw.Event
import CyBy.Draw.MoleculeCanvas
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Extensions.Util

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
exportImageToWord : String -> JSIO ()
exportImageToWord s = primIO $ prim__exportImageToWord s

fromClipboard : Cmd DrawEvent
fromClipboard =
  C $ \h => primIO $ prim__importImageFromWord $ \s,w =>
    case extractMetadata s of
      Left e  => toPrim (runJS $ h (Msg $ ReadErr e)) w
      Right m => toPrim (runJS $ h (SetTempl (readMolfile m))) w


||| Parses a word event and forms a DrawEvent command.
export
dispWordExt : DrawSettings => ExtensionEvent -> DrawState -> Cmd DrawEvent
dispWordExt ExportSVG s = cmd_ (exportImageToWord $ exportSVG s)
dispWordExt ImportSVG s = fromClipboard
