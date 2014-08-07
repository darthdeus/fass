module Fass.Parser.Value where

import Fass.Parser.Color

data Units = Pixel
           | Em
           | Ex
           | Vh

data Value = Color RGBA
           | Size Int Units
