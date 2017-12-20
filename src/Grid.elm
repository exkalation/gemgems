module Grid exposing (..)

import Dict
import Hexagons.Hex as Hex exposing (Direction(..))
import Hexagons.Map as HexMap
import Hexagons.Layout as HexL

import Gem


drawPosition layout hex =
    let (x, y) = HexL.hexToPoint layout hex
    in (y, x)

getDiffY layout fromHex toHex =
    let
        (_, y1) = drawPosition layout fromHex
        (_, y2) = drawPosition layout toHex
    in
        y2 - y1

buildBottomLine : Hex.Hex -> Dict.Dict HexMap.Hash Gem.Gem -> List Hex.Hex
buildBottomLine hex gems =
    let
        neighborExists direction = getNeighborGem hex direction gems /= Nothing
        upLeft = neighborExists SE
        up = neighborExists SW
        downRight = neighborExists NW
        down = neighborExists NE
        downLeft = neighborExists E
        go =     if upLeft && down then Just (Nothing, SE) -- go upLeft
            else if not upLeft && not up && downLeft then Just (Nothing, E) -- go downLeft
            else if not upLeft && down && not downLeft then Just (Nothing, NE) -- go down
            else if up && not down && downRight then Just (Just hex, NW) -- add and go downRight
            else if upLeft && up && not down && not downRight then Just (Just hex, W) -- = bottom line, lower --> add and go upRight
            else Nothing -- end
    in
        case go of
            Just (Just addHex, direction) -> addHex :: (buildBottomLine (Hex.neighbor hex direction) gems)
            Just (Nothing, direction) -> buildBottomLine (Hex.neighbor hex direction) gems
            Nothing -> []

getNeighborGem hex direction gems =
    Dict.get (Hex.neighbor hex direction |> HexMap.hashHex) gems