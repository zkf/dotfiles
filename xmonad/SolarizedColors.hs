module SolarizedColors
( solarizedLight
, solarizedDark
, Solarized(..)
, yellow
, orange
, red
, magenta
, violet
, blue
, cyan
, green)
where

type Color = String
base03, base02, base01, base00, base0, base1, base2, base3 :: Color
yellow, orange, red, magenta, violet, blue, cyan, green :: Color
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"

yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

data Solarized = Solarized
                   { primaryContent :: Color
                   , secondaryContent :: Color
                   , emphasizedContent :: Color
                   , background :: Color
                   , bgHighlights :: Color }

solarizedLight :: Solarized
solarizedLight = Solarized
                   { primaryContent = base00
                   , secondaryContent = base1
                   , emphasizedContent = base01
                   , background = base3
                   , bgHighlights = base2 }

solarizedDark :: Solarized
solarizedDark = Solarized
                  { primaryContent = base0
                  , secondaryContent = base01
                  , emphasizedContent = base1
                  , background = base03
                  , bgHighlights = base02 }
