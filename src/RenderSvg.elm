module RenderSvg exposing (..)

import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (..)

base : (Float, Float) -> String
base (dx, dy) =
    [ (30.1,84.5), (10.2,50), (30.1,15.5), (69.9,15.5), (89.8,50), (69.9,84.5) ]
        |> List.map (\(a, b) -> (toString (a + dx)) ++ "," ++ (toString (b + dy)))
        |> List.foldl (\a b -> if b /= "" then a ++ " " ++ b else a) ""

rectGem (b, a) props =
    let
        p = base (a, b)
    in
    --rect (List.append [ SvgA.x (toString (a*scale)), SvgA.y (toString (b*scale)), SvgA.width "30", SvgA.height "30", SvgA.rx "5", SvgA.ry "5" ] props) []
    --Svg.path List.append props [ SvgA.transform "scale(0.4)", fill "#ff0", SvgA.x (toString a), SvgA.y (toString b), d "M2.5000000000000004 47.63139720814412Q0 43.30127018922193 2.5000000000000004 38.97114317029974L22.5 4.330127018922193Q25 0 30 0L70 0Q75 0 77.5 4.330127018922193L97.5 38.97114317029974Q100 43.30127018922193 97.5 47.63139720814412L77.5 82.27241335952166Q75 86.60254037844386 70 86.60254037844386L30 86.60254037844386Q25 86.60254037844386 22.5 82.27241335952166Z" ] []
    polygon (List.append props [ transform "scale(1)", SvgA.x (toString a), SvgA.y (toString b), points p ]) []

redSquare (a, b) msgDown msgUp =
    rectGem (a, b) [ fill "#f00", onMouseDown msgDown, onMouseUp msgUp ]
blueSquare (a, b) msgDown msgUp =
    rectGem (a, b) [ fill "#0B79CE", onMouseDown msgDown, onMouseUp msgUp ]
blackSquare (a, b) msgDown msgUp =
    rectGem (a, b) [ fill "#000", onMouseDown msgDown, onMouseUp msgUp ]
greenSquare (a, b) msgDown msgUp =
    rectGem (a, b) [ fill "#0f0", onMouseDown msgDown, onMouseUp msgUp ]
highlightSquare (a, b) msgDown msgUp =
    rectGem (a, b) [ fill "#fff", onMouseDown msgDown, onMouseUp msgUp ]

drawContainer =
    svg [ width "600", height "600", viewBox "0 0 600 600" ]

drawShape el ab msgDown msgUp =
    case el of
        Just 1 -> redSquare ab msgDown msgUp
        Just 2 -> blueSquare ab msgDown msgUp
        Just 3 -> greenSquare ab msgDown msgUp
        Just 4 -> blackSquare ab msgDown msgUp --redCircle ab
        --Just 5 -> blueCircle ab msgDown msgUp
        --Just 6 -> greenCircle ab msgDown msgUp
        Just 9 -> highlightSquare ab msgDown msgUp
        Nothing -> redSquare ab msgDown msgUp
        default -> Debug.crash "Unhandled shape tpye!"

--circleGem (b, a) props =
--    circle (List.append [ SvgA.cx (toString (a+15)), SvgA.cy (toString (b+15)), SvgA.r "15" ] props) []
--redCircle (a, b) =
--    circleGem (a, b) [ fill "#f00" ]
--blueCircle (a, b) =
--    circleGem (a, b) [ fill "#0B79CE" ]
--greenCircle (a, b) =
--    circleGem (a, b) [ fill "#0f0" ]