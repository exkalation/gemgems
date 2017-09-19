module Gem exposing (..)

type Gem = Empty | Dragged | Color Int

isColor gem =
    case (gem) of
        Color x -> True
        default -> False
isDragged gem =
    gem == Dragged
isEmpty gem =
    gem == Empty
