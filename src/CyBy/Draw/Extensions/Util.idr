module CyBy.Draw.Extensions.Util

import Data.SnocList

%default total

public export
data ExtensionEvent = ExportSVG | ImportSVG

-- extracts the content of the `<metadata>` tag
export
extractMetadata : String -> Either String String
extractMetadata str =
   map (pack . cast . fst) $ collectMetadata Lin (unpack str) False
  where
    collectMetadata : SnocList Char -> List Char -> Bool -> Either String (SnocList Char, List Char)
    collectMetadata acc ('<' :: 'm' :: 'e' :: 't' :: 'a' :: 'd' :: 'a' :: 't' :: 'a' :: '>' :: xs) _ = collectMetadata acc xs True
    collectMetadata acc ('<' :: '/' :: 'm' :: 'e' :: 't' :: 'a' :: 'd' :: 'a' :: 't' :: 'a' :: '>' :: xs) _ = Right (acc,[])
    collectMetadata acc (x :: xs) True  = collectMetadata (acc :< x) xs True
    collectMetadata acc (x :: xs) False = collectMetadata acc        xs False
    collectMetadata acc [] _            =
      Left "No metadata or no end tag was found during metadata extraction!"
