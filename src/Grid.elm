module Grid exposing (..)

import Hexagons.Layout as HexL exposing (orientationLayoutPointy)

drawPosition layout hex =
    let (x, y) = HexL.hexToPoint layout hex
    in (y, x)