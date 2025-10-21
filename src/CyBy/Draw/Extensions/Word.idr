||| This module takes care of all events used for the word add-in (extension)
module CyBy.Draw.Extensions.Word

import PrimIO

import Web.Dom
import Web.MVC
import Web.Html

import Data.Graph.Indexed
import CyBy.Draw.Event
import CyBy.Draw.MoleculeCanvas
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Extensions.Util

import CyBy.Draw.Extensions.DomBindings2


%default total


-- helper function for removing unused XML objects of deleted images
checkValidXmlObjects : String
checkValidXmlObjects =
  """
    Word.run(async (context) => {
      // check first if some images were deleted
      // get the whole doc as XML
      xmlDocRaw = context.document.body.getOoxml();
      await context.sync();
      const psr = new DOMParser();
      const xmlDoc = psr.parseFromString(xmlDocRaw.value,'text/xml');

      // search for the shapes and inlinePictures and collect
      // the id's
      const docPrElems = xmlDoc.getElementsByTagName("wp:docPr");
      const ids = [];
      if (docPrElems.length > 0) {
        for (let i = 0; i < docPrElems.length; i++) {
          const descr = docPrElems[i].getAttribute('descr');
          if (descr && descr.startsWith('cyby_draw_img_')) {
            ids.push(descr);
          }
        }
      }

      // load custom XML objects and search for `graphInfo` parts
      const allCustomXmlParts = context.document.customXmlParts;
      allCustomXmlParts.load("items");
      await context.sync();

      for (const elem of allCustomXmlParts.items) {
        const queryRes = elem.query("//graphInfo/id",{});
        await context.sync();
        if (queryRes.value[0] === undefined) {
          continue;
        }
        // delete the XML object if the id was not found earlier,
        // meaning the image was deleted
        const val = queryRes.value[0];
        const match = val.match(new RegExp("<id>(.*?)</id>"));
        if (queryRes.value.length > 0) {
          if (!ids.includes(match[1])) {
            elem.delete();
          }
        }
      }
    });
  """


-- uses the Word-API for exporting an svg string to a word document
-- image (inlinePicture)
-- to preserve the graph, the image is labeled with an unique id
-- in the alternative text, referencing the mol file inside the newly
-- created XML object
--
-- async / await is needed for promises, see
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
-- for further informations
%foreign
  """
  browser:lambda:(s,g,removeUnusedXmlObjects,w) => {
    Word.run(async (context) => {
      // clean up unused XML objects
      eval(removeUnusedXmlObjects);

      // add the picture and create a corresponding XML object for it
      // encoding the picture
      const b64 = btoa(s);
      // get the selection range
      const selection = context.document.getSelection();
      // add the picture to the selection and store it in a variable
      const image = selection.insertInlinePictureFromBase64(b64, Word.InsertLocation.end);
      context.trackedObjects.add(image);
      // reload the selected image
      image.load();

      // generating a unique index for the picture
      const id = 'cyby_draw_img_' + Date.now() + Math.floor(Math.random() * 10000);

      // add the alt text to the image and end tracking of the object
      image.altTextDescription = id;
      context.sync();
      context.trackedObjects.remove(image);
      context.sync();

      // creating and adding an XML object with the associated id
      const xmlContent = `<graphInfo><id>${id}</id><graph>${g}</graph></graphInfo>`;
      context.document.customXmlParts.add(xmlContent);
      context.sync();
    });
  }
  """
prim__exportImageToWord : (svg,molFile,helperF : String) -> PrimIO ()

-- extracting the mol file from a molecular structure image (created by CyBy-Draw)
-- for editing it in the CyBy-Draw editor
-- async / await is needed for promises, see
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
-- for further informations
%foreign
  """
  browser:lambda:(removeUnusedXmlObjects,f,w) => {
    Word.run(async context => {
      // clean up unused XML objects
      eval(removeUnusedXmlObjects);

      // retrieve the mol file of the selected picture
      // load the selected parts of the word file
      const selection = context.document.getSelection();

      // return an empty string if nothing is selected
      selection.load('isEmpty');
      await context.sync();
      if (selection.isEmpty) {
        console.log('No images selected!');
        return f('')(w);
      }

      let id = null;

      // to find a selected shape ID, the word document has to be
      // searched in the XML form as `selection.shapes` is not
      // integrated in the current Word API
      const ooxml = selection.getOoxml();
      await context.sync();

      const parser = new DOMParser();
      const xmlSel = parser.parseFromString(ooxml.value,'text/xml');
    
      // search for the shapes and inlinePictures labeled with a cyby id
      const selPrElems = xmlSel.getElementsByTagName("wp:docPr");
      if (selPrElems.length > 0) {
        for (let i = 0; i < selPrElems.length; i++) {
          const descr = selPrElems[i].getAttribute('descr');
          if (descr && descr.startsWith('cyby_draw_img_')) {
            id = descr;
            break;
          }
        }
      }

      // extract the mol file data (of the selected image) by id
      // from the stored XML objects
      const customXmlParts = context.document.customXmlParts;
      customXmlParts.load('items');
      await context.sync();

      for (const part of customXmlParts.items) {
        const partID = part.query(`//graphInfo[id='${id}']/graph`, {});
        await context.sync();

        if (partID.value[0] === undefined) {
          continue;
        } else {
          return f(partID.value[0])(w);
        }
      }

      // if no ID matches, return an empty string
      // this can happen if the alternative text for the
      // selected image, which represents the ID, was altered
      console.log('No value found for this image(ID)! This can happen if the alternative text for the image was manually changed!');
      return f('')(w);
    });
  }
  """
prim__importImageFromWord : (helperF : String) -> (String -> PrimIO ()) -> PrimIO ()

%inline
exportImageToWord : (svg,molFile : String) -> JSIO ()
exportImageToWord s mol = primIO $ prim__exportImageToWord s mol checkValidXmlObjects




exportImageToWord' : (svg,molFile : String) -> JSIO ()
exportImageToWord' svg mol = do
  wordRun $ \c => do
    doc <- document c
    bdy <- body doc
    prom1 <- promise bdy (insText "test")
    ?foo



fromClipboard : Cmd DrawEvent
fromClipboard =
  C $ \h => primIO $ prim__importImageFromWord checkValidXmlObjects $ \s,w =>
    case extractAndParseMetadata s of
      Left e  => toPrim (runJS $ h (Msg $ ReadErr e)) w
      Right m => toPrim (runJS $ h (SetTempl m)) w


||| Parses a word event and forms a DrawEvent command.
export
dispWordExt : DrawSettings => ExtensionEvent -> DrawState -> Cmd DrawEvent
dispWordExt ExportSVG s = cmd_ $ exportImageToWord' (exportSVG s) (toMolStr s)
dispWordExt ImportSVG s = fromClipboard
