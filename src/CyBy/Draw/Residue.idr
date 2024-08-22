||| Corresponds to residues found in `ChemDoodle.RESIDUE`
module CyBy.Draw.Residue

%default total


||| I did not incluede 'acidity', because we don't need it later.
||| (claudio-etterli)
record Info where
  constructor MkInfo
  symbol : String
  name : String
  polar : Bool
  aminoColor : String
  shaplyColor : String


||| '*' problematic as a type, therefore I named it 'Other'
data Residue = Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile |
               Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val |
               Asx | Glx | A   | G   | I   | C   | T   | U   | Other

public export
residue : Residue -> Info
residue Ala   = MkInfo "Ala" "Alanine" False "#C8C8C8" "#8CFF8C"
residue Arg   = MkInfo "Arg" "Arginine" True "#145AFF" "#00007C"
residue Asn   = MkInfo "Asn" "Asparagine" True "#00DCDC" "#FF7C70"
residue Asp   = MkInfo "Asp" "Aspartic Acid" True "#E60A0A" "#A00042"
residue Cys   = MkInfo "Cys" "Cysteine" True "#E6E600" "#FFFF70"
residue Gln   = MkInfo "Gln" "Glutamine" True "#00DCDC" "#FF4C4C"
residue Glu   = MkInfo "Glu" "Glutamic Acid" True "#E60A0A" "#660000"
residue Gly   = MkInfo "Gly" "Glycine" False "#EBEBEB" "#FFFFFF"
residue His   = MkInfo "His" "Histidine" True "#8282D2" "#7070FF"
residue Ile   = MkInfo "Ile" "Isoleucine" False "#0F820F" "#004C00"
residue Leu   = MkInfo "Leu" "Leucine" False "#0F820F" "#455E45"
residue Lys   = MkInfo "Lys" "Lysine" True "#145AFF" "#4747B8"
residue Met   = MkInfo "Met" "Methionine" False "#E6E600" "#B8A042"
residue Phe   = MkInfo "Phe" "Phenylalanine" False "#3232AA" "#534C52"
residue Pro   = MkInfo "Pro" "Proline" False "#DC9682" "#525252"
residue Ser   = MkInfo "Ser" "Serine" True "#FA9600" "#FF7042"
residue Thr   = MkInfo "Thr" "Threonine" True "#FA9600" "#B84C00"
residue Trp   = MkInfo "Trp" "Tryptophan" True "#B45AB4" "#4F4600"
residue Tyr   = MkInfo "Tyr" "Tyrosine" True "#3232AA" "#8C704C"
residue Val   = MkInfo "Val" "Valine" False "#0F820F" "#FF8CFF"
residue Asx   = MkInfo "Asx" "Asparagine/Aspartic Acid" True "#FF69B4" "#FF00FF"
residue Glx   = MkInfo "Glx" "Glutamine/Glutamic Acid" True "#FF69B4" "#FF00FF"
residue A     = MkInfo "A" "Adenine" False "#BEA06E" "#A0A0FF"
residue G     = MkInfo "G" "Guanine" False "#BEA06E" "#FF7070"
residue I     = MkInfo "I" "" False "#BEA06E" "#80FFFF"
residue C     = MkInfo "C" "Cytosine" False "#BEA06E" "#FF8C4B"
residue T     = MkInfo "T" "Thymine" False "#BEA06E" "#A0FFA0"
residue U     = MkInfo "U" "Uracil" False "#BEA06E" "#FF8080"
residue Other = MkInfo "*" "Other" False "#BEA06E" "#FF00FF"

namespace Res
  public export %inline
  symbol : Residue -> String
  symbol = symbol . residue

  public export %inline
  name : Residue -> String
  name = name . residue

  public export %inline
  polar : Residue -> Bool
  polar = polar . residue

  public export %inline
  aminoColor : Residue -> String
  aminoColor = aminoColor . residue

  public export %inline
  shaplyColor : Residue -> String
  shaplyColor = shaplyColor . residue
