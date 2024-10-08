module CyBy.Draw.Internal.Color

import Text.Molfile
import Text.SVG.Types

%default total

export
jmolColor : Elem -> SVGColor
jmolColor H  = RGB 0xFF 0xFF 0xFF
jmolColor He = RGB 0xD9 0xFF 0xFF
jmolColor Li = RGB 0xCC 0x80 0xFF
jmolColor Be = RGB 0xC2 0xFF 0x00
jmolColor B  = RGB 0xFF 0xB5 0xB5
jmolColor C  = RGB 0x90 0x90 0x90
jmolColor N  = RGB 0x30 0x50 0xF8
jmolColor O  = RGB 0xFF 0x0D 0x0D
jmolColor F  = RGB 0x90 0xE0 0x50
jmolColor Ne = RGB 0xB3 0xE3 0xF5
jmolColor Na = RGB 0xAB 0x5C 0xF2
jmolColor Mg = RGB 0x8A 0xFF 0x00
jmolColor Al = RGB 0xBF 0xA6 0xA6
jmolColor Si = RGB 0xF0 0xC8 0xA0
jmolColor P  = RGB 0xFF 0x80 0x00
jmolColor S  = RGB 0xFF 0xFF 0x30
jmolColor Cl = RGB 0x1F 0xF0 0x1F
jmolColor Ar = RGB 0x80 0xD1 0xE3
jmolColor K  = RGB 0x8F 0x40 0xD4
jmolColor Ca = RGB 0x3D 0xFF 0x00
jmolColor Sc = RGB 0xE6 0xE6 0xE6
jmolColor Ti = RGB 0xBF 0xC2 0xC7
jmolColor V  = RGB 0xA6 0xA6 0xAB
jmolColor Cr = RGB 0x8A 0x99 0xC7
jmolColor Mn = RGB 0x9C 0x7A 0xC7
jmolColor Fe = RGB 0xE0 0x66 0x33
jmolColor Co = RGB 0xF0 0x90 0xA0
jmolColor Ni = RGB 0x50 0xD0 0x50
jmolColor Cu = RGB 0xC8 0x80 0x33
jmolColor Zn = RGB 0x7D 0x80 0xB0
jmolColor Ga = RGB 0xC2 0x8F 0x8F
jmolColor Ge = RGB 0x66 0x8F 0x8F
jmolColor As = RGB 0xBD 0x80 0xE3
jmolColor Se = RGB 0xFF 0xA1 0x00
jmolColor Br = RGB 0xA6 0x29 0x29
jmolColor Kr = RGB 0x5C 0xB8 0xD1
jmolColor Rb = RGB 0x70 0x2E 0xB0
jmolColor Sr = RGB 0x00 0xFF 0x00
jmolColor Y  = RGB 0x94 0xFF 0xFF
jmolColor Zr = RGB 0x94 0xE0 0xE0
jmolColor Nb = RGB 0x73 0xC2 0xC9
jmolColor Mo = RGB 0x54 0xB5 0xB5
jmolColor Tc = RGB 0x3B 0x9E 0x9E
jmolColor Ru = RGB 0x24 0x8F 0x8F
jmolColor Rh = RGB 0x0A 0x7D 0x8C
jmolColor Pd = RGB 0x00 0x69 0x85
jmolColor Ag = RGB 0xC0 0xC0 0xC0
jmolColor Cd = RGB 0xFF 0xD9 0x8F
jmolColor In = RGB 0xA6 0x75 0x73
jmolColor Sn = RGB 0x66 0x80 0x80
jmolColor Sb = RGB 0x9E 0x63 0xB5
jmolColor Te = RGB 0xD4 0x7A 0x00
jmolColor I  = RGB 0x94 0x00 0x94
jmolColor Xe = RGB 0x42 0x9E 0xB0
jmolColor Cs = RGB 0x57 0x17 0x8F
jmolColor Ba = RGB 0x00 0xC9 0x00
jmolColor La = RGB 0x70 0xD4 0xFF
jmolColor Ce = RGB 0xFF 0xFF 0xC7
jmolColor Pr = RGB 0xD9 0xFF 0xC7
jmolColor Nd = RGB 0xC7 0xFF 0xC7
jmolColor Pm = RGB 0xA3 0xFF 0xC7
jmolColor Sm = RGB 0x8F 0xFF 0xC7
jmolColor Eu = RGB 0x61 0xFF 0xC7
jmolColor Gd = RGB 0x45 0xFF 0xC7
jmolColor Tb = RGB 0x30 0xFF 0xC7
jmolColor Dy = RGB 0x1F 0xFF 0xC7
jmolColor Ho = RGB 0x00 0xFF 0x9C
jmolColor Er = RGB 0x00 0xE6 0x75
jmolColor Tm = RGB 0x00 0xD4 0x52
jmolColor Yb = RGB 0x00 0xBF 0x38
jmolColor Lu = RGB 0x00 0xAB 0x24
jmolColor Hf = RGB 0x4D 0xC2 0xFF
jmolColor Ta = RGB 0x4D 0xA6 0xFF
jmolColor W  = RGB 0x21 0x94 0xD6
jmolColor Re = RGB 0x26 0x7D 0xAB
jmolColor Os = RGB 0x26 0x66 0x96
jmolColor Ir = RGB 0x17 0x54 0x87
jmolColor Pt = RGB 0xD0 0xD0 0xE0
jmolColor Au = RGB 0xFF 0xD1 0x23
jmolColor Hg = RGB 0xB8 0xB8 0xD0
jmolColor Tl = RGB 0xA6 0x54 0x4D
jmolColor Pb = RGB 0x57 0x59 0x61
jmolColor Bi = RGB 0x9E 0x4F 0xB5
jmolColor Po = RGB 0xAB 0x5C 0x00
jmolColor At = RGB 0x75 0x4F 0x45
jmolColor Rn = RGB 0x42 0x82 0x96
jmolColor Fr = RGB 0x42 0x00 0x66
jmolColor Ra = RGB 0x00 0x7D 0x00
jmolColor Ac = RGB 0x70 0xAB 0xFA
jmolColor Th = RGB 0x00 0xBA 0xFF
jmolColor Pa = RGB 0x00 0xA1 0xFF
jmolColor U  = RGB 0x00 0x8F 0xFF
jmolColor Np = RGB 0x00 0x80 0xFF
jmolColor Pu = RGB 0x00 0x6B 0xFF
jmolColor Am = RGB 0x54 0x5C 0xF2
jmolColor Cm = RGB 0x78 0x5C 0xE3
jmolColor Bk = RGB 0x8A 0x4F 0xE3
jmolColor Cf = RGB 0xA1 0x36 0xD4
jmolColor Es = RGB 0xB3 0x1F 0xD4
jmolColor Fm = RGB 0xB3 0x1F 0xBA
jmolColor Md = RGB 0xB3 0x0D 0xA6
jmolColor No = RGB 0xBD 0x0D 0x87
jmolColor Lr = RGB 0xC7 0x00 0x66
jmolColor Rf = RGB 0xCC 0x00 0x59
jmolColor Db = RGB 0xD1 0x00 0x4F
jmolColor Sg = RGB 0xD9 0x00 0x45
jmolColor Bh = RGB 0xE0 0x00 0x38
jmolColor Hs = RGB 0xE6 0x00 0x2E
jmolColor Mt = RGB 0xEB 0x00 0x26
jmolColor Ds = RGB 0x00 0x00 0x00
jmolColor Rg = RGB 0x00 0x00 0x00
jmolColor Cn = RGB 0x00 0x00 0x00
jmolColor Nh = RGB 0x00 0x00 0x00
jmolColor Fl = RGB 0x00 0x00 0x00
jmolColor Mc = RGB 0x00 0x00 0x00
jmolColor Lv = RGB 0x00 0x00 0x00
jmolColor Ts = RGB 0x00 0x00 0x00
jmolColor Og = RGB 0x00 0x00 0x00

export
pymolColor : Elem -> SVGColor
pymolColor H = RGB 0xE6 0xE6 0xE6
pymolColor C = RGB 0x33 0xFF 0x33
pymolColor N = RGB 0x33 0x33 0xFF
pymolColor O = RGB 0xFF 0x4D 0x4D
pymolColor F = RGB 0xB3 0xFF 0xFF
pymolColor S = RGB 0xE6 0xC6 0x40
pymolColor e = jmolColor e

export
cybyColor : Elem -> SVGColor
cybyColor H = silver
cybyColor C = dimgray
cybyColor F = limegreen
cybyColor S = RGB 0xE6 0xC6 0x40
cybyColor e = jmolColor e

-- Colors from CPK, ugly as they come
-- Adpated H
export
cpkColor : Elem -> SVGColor
cpkColor H  = silver -- cdk: white
cpkColor He = RGB 0xFF 0xC0 0xCB
cpkColor Li = RGB 0xB2 0x22 0x22
cpkColor B  = RGB 0x00 0xFF 0x00
cpkColor C  = RGB 0xC8 0xC8 0xC8
cpkColor N  = RGB 0x8F 0x8F 0xFF
cpkColor O  = RGB 0xF0 0x00 0x00
cpkColor F  = RGB 0xDA 0xA5 0x20
cpkColor Na = RGB 0x00 0x00 0xFF
cpkColor Mg = RGB 0x22 0x8B 0x22
cpkColor Al = RGB 0x80 0x80 0x90
cpkColor Si = RGB 0xDA 0xA5 0x20
cpkColor P  = RGB 0xFF 0xA5 0x00
cpkColor S  = RGB 0xFF 0xC8 0x32
cpkColor Cl = RGB 0x00 0xFF 0x00
cpkColor Ca = RGB 0x80 0x80 0x90
cpkColor Ti = RGB 0x80 0x80 0x90
cpkColor Cr = RGB 0x80 0x80 0x90
cpkColor Mn = RGB 0x80 0x80 0x90
cpkColor Fe = RGB 0xFF 0xA5 0x00
cpkColor Ni = RGB 0xA5 0x2A 0x2A
cpkColor Cu = RGB 0xA5 0x2A 0x2A
cpkColor Zn = RGB 0xA5 0x2A 0x2A
cpkColor Br = RGB 0xA5 0x2A 0x2A
cpkColor Ag = RGB 0x80 0x80 0x90
cpkColor I  = RGB 0xA0 0x20 0xF0
cpkColor Ba = RGB 0xFF 0xA5 0x00
cpkColor Au = RGB 0xDA 0xA5 0x20
cpkColor _  = RGB 0xFF 0x14 0x93

-- Colors as defined in CDKAtomColors
-- Adapted Color for H and S
export 
cdkColor : Elem -> SVGColor
cdkColor H  = silver -- cdk: white
cdkColor C  = black
cdkColor N  = blue
cdkColor O  = red
cdkColor P  = green
cdkColor S  = gold -- cdk: yellow
cdkColor Cl = magenta
cdkColor _  = RGB 0x48 0x48 0x48

-- Coloring of important Elements
-- The colors are:
-- - H:  lightgrey (RGB 0x90 0x90 0x90)
-- - C:  black 
-- - N:  blue (RGB 0x0b 0x53 0x94)
-- - O:  red (RGB 0xcc 0x00 0x00)
-- - P:  brown (RGB 0xb4 0x5f 0x06)
-- - S:  yellow (RGB 0xe5 0xae 0x06) 
-- - Cl: green 
export 
basicColors : Elem -> SVGColor
basicColors H  = RGB 0x90 0x90 0x90
basicColors C  = black
basicColors N  = RGB 0x0b 0x53 0x94
basicColors O  = RGB 0xcc 0x00 0x00
basicColors P  = RGB 0xb4 0x5f 0x06
basicColors S  = RGB 0xe5 0xae 0x06  
basicColors Cl = green 
basicColors _  = RGB 0x48 0x48 0x48

-- Colored PSE groups
-- Addtionally to the specific coloring of important elements, the elements are 
-- colored according to the group they are in. 
-- The colors are: 
-- - alkali metals:         dark pink (RGB 0xa6 0x4d 0x79)
-- - alkaline earth metals: pink (RGB 0xc2 0x7b 0xa0)
-- - transition metals:     purple (RGB 0x9c 0x7a 0xc7)
-- - metals:                turquoise (RGB 0x42 0x9e 0xb0)
-- - metalloids:            blue (RGB 0x0b 0x53 0x94)
-- - nonmetals:             coloring of significnat elemets see basicColors
-- - halogens:              light green (RGB 0x0a 0x72 0x48)
-- - noble gases:           green (RGB 0x06 0x55 0x35)
export
groupColors : Elem -> SVGColor
groupColors H  = RGB 0x90 0x90 0x90
groupColors C  = black
groupColors N  = RGB 0x0b 0x53 0x94
groupColors O  = RGB 0xcc 0x00 0x00
groupColors P  = RGB 0xb4 0x5f 0x06
groupColors S  = gold
groupColors Cl = RGB 0x3a 0x8e 0x6c 
groupColors He = RGB 0x06 0x55 0x35
groupColors Ne = RGB 0x06 0x55 0x35
groupColors Ar = RGB 0x06 0x55 0x35
groupColors Kr = RGB 0x06 0x55 0x35
groupColors Xe = RGB 0x06 0x55 0x35
groupColors Rn = RGB 0x06 0x55 0x35
groupColors Og = RGB 0x06 0x55 0x35
groupColors F  = RGB 0x0a 0x72 0x48
groupColors Br = RGB 0x0a 0x72 0x48
groupColors I  = RGB 0x0a 0x72 0x48
groupColors At = RGB 0x0a 0x72 0x48
groupColors Ts = RGB 0x0a 0x72 0x48
groupColors B  = RGB 0x0b 0x53 0x94
groupColors Si = RGB 0x0b 0x53 0x94
groupColors Ge = RGB 0x0b 0x53 0x94
groupColors As = RGB 0x0b 0x53 0x94
groupColors Sb = RGB 0x0b 0x53 0x94
groupColors Se = RGB 0x0b 0x53 0x94
groupColors Te = RGB 0x0b 0x53 0x94
groupColors Po = RGB 0x0b 0x53 0x94
groupColors Al = RGB 0x42 0x9e 0xb0
groupColors Ga = RGB 0x42 0x9e 0xb0
groupColors In = RGB 0x42 0x9e 0xb0
groupColors Tl = RGB 0x42 0x9e 0xb0
groupColors Nh = RGB 0x42 0x9e 0xb0
groupColors Sn = RGB 0x42 0x9e 0xb0
groupColors Pb = RGB 0x42 0x9e 0xb0
groupColors Fl = RGB 0x42 0x9e 0xb0
groupColors Bi = RGB 0x42 0x9e 0xb0
groupColors Mc = RGB 0x42 0x9e 0xb0
groupColors Lv = RGB 0x42 0x9e 0xb0
groupColors Be = RGB 0xc2 0x7b 0xa0 
groupColors Ca = RGB 0xc2 0x7b 0xa0 
groupColors Mg = RGB 0xc2 0x7b 0xa0
groupColors Sr = RGB 0xc2 0x7b 0xa0 
groupColors Ba = RGB 0xc2 0x7b 0xa0
groupColors Ra = RGB 0xc2 0x7b 0xa0
groupColors Li = RGB 0xa6 0x4d 0x79 
groupColors Na = RGB 0xa6 0x4d 0x79 
groupColors K  = RGB 0xa6 0x4d 0x79
groupColors Rb = RGB 0xa6 0x4d 0x79 
groupColors Cs = RGB 0xa6 0x4d 0x79
groupColors Fr = RGB 0xa6 0x4d 0x79
groupColors Ce = RGB 0x72 0x70 0x74
groupColors Pr = RGB 0x72 0x70 0x74
groupColors Nd = RGB 0x72 0x70 0x74
groupColors Pm = RGB 0x72 0x70 0x74
groupColors Sm = RGB 0x72 0x70 0x74
groupColors Eu = RGB 0x72 0x70 0x74
groupColors Gd = RGB 0x72 0x70 0x74
groupColors Tb = RGB 0x72 0x70 0x74
groupColors Dy = RGB 0x72 0x70 0x74
groupColors Ho = RGB 0x72 0x70 0x74
groupColors Er = RGB 0x72 0x70 0x74
groupColors Tm = RGB 0x72 0x70 0x74
groupColors Yb = RGB 0x72 0x70 0x74
groupColors Lu = RGB 0x72 0x70 0x74
groupColors Th = RGB 0x48 0x48 0x48
groupColors Pa = RGB 0x48 0x48 0x48
groupColors U  = RGB 0x48 0x48 0x48 
groupColors Np = RGB 0x48 0x48 0x48
groupColors Pu = RGB 0x48 0x48 0x48
groupColors Am = RGB 0x48 0x48 0x48
groupColors Cm = RGB 0x48 0x48 0x48
groupColors Bk = RGB 0x48 0x48 0x48
groupColors Cf = RGB 0x48 0x48 0x48
groupColors Es = RGB 0x48 0x48 0x48
groupColors Fm = RGB 0x48 0x48 0x48
groupColors Md = RGB 0x48 0x48 0x48
groupColors No = RGB 0x48 0x48 0x48
groupColors Lr = RGB 0x48 0x48 0x48
groupColors _  = RGB 0x9c 0x7a 0xc7
