module RenderSvg exposing (..)

import Json.Decode as Json exposing (succeed)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (..)
import Gem exposing (..)


base : String
base =
    [ (30.1,84.5), (10.2,50), (30.1,15.5), (69.9,15.5), (89.8,50), (69.9,84.5) ]
    --[ (32.500,0.000), (16.250,28.146), (-16.250,28.146), (-32.500,0.000), (-16.250,-28.146), (16.250,-28.146) ]
        --|> List.map (\(a, b) -> (toString (0.95*a + dx)) ++ "," ++ (toString (0.95*b + dy)))
        |> List.map (\(a, b) -> (toString a) ++ "," ++ (toString b))
        --|> String.join "," --<<-- try!!
        |> List.foldl (\a b -> if b /= "" then a ++ " " ++ b else a) ""

onMouseLeave msg =
    on "mouseLeave" (Json.succeed msg)

drawContainer =
    svg [ width "600", height "600", viewBox "0 0 600 600" ]

shapeTypeToColor gemType =
    case gemType of
        Just (Gem.Color 1) -> "#9B111E" -- ruby red #9B111E / red #f00
        Just (Gem.Color 2) -> "#0c0" -- emerald #50C878 / green #0c0
        Just (Gem.Color 3) -> "#0F52BA" -- sapphire #0F52BA / blue #07a
        Just (Gem.Color 4) -> "#ff0" -- yellow
        Just (Gem.Color 5) -> "#f30" -- orange
        Just (Gem.Color 6) -> "#60c" -- violet
        Just Gem.Dragged -> "#fff" -- dragged
        Just Gem.Empty -> "#aaa" -- eliminated/empty
        Nothing -> "#000"
        default -> Debug.crash "Unhandled shape tpye!"

drawShape gemType (x, y) msgDown msgUp =
    let
        (dx, dy) = (y, x)
        color = shapeTypeToColor gemType
    in
        g [ transform ("translate(" ++ (toString dx) ++ "," ++ (toString dy) ++ ") scale(0.91)") ]
        [ polygon [ fill color, SvgA.stroke "#000", onMouseDown msgDown, onMouseUp msgUp, points base ] []
        ]

