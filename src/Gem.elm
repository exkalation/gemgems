module Gem exposing (..)

type Gem = Empty | Dragged | Matched Int | Color Int

isColor gem =
    case (gem) of
        Color _ -> True
        default -> False

isDragged gem =
    gem == Dragged

isEmpty gem =
    case gem of
        Empty -> True
        Matched _ -> True
        default -> False

colorToMatched gem =
    case gem of
        Color x -> Matched x
        default -> Debug.crash "colorToMatched - gem must be a color!" -- Gem.Empty

toStringElmBugWorkaround a =
        String.repeat a "1"

toString gem =
    case gem of
        Color c -> "Color-" ++ (toStringElmBugWorkaround c)
        Matched c -> "Matched-" ++ (toStringElmBugWorkaround c)
        Empty -> "Empty"
        Dragged -> "Dragged"