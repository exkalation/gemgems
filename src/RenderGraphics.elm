module RenderGraphics exposing (..)

import Collage exposing (..)
import Color exposing (..)

scale = 1
{-
rectGem (b, a) props =
    rect (List.append [ SvgA.x (toString (a*scale)), SvgA.y (toString (b*scale)), SvgA.width "30", SvgA.height "30", SvgA.rx "5", SvgA.ry "5" ] props) []
circleGem (b, a) props =
    circle (List.append [ SvgA.cx (toString (a+15)), SvgA.cy (toString (b+15)), SvgA.r "15" ] props) []
redSquare (a, b) =
    rectGem (a, b) [ fill "#f00" ]
blueSquare (a, b) =
    rectGem (a, b) [ fill "#0B79CE" ]
greenSquare (a, b) =
    rectGem (a, b) [ fill "#0f0" ]
redCircle (a, b) =
    circleGem (a, b) [ fill "#f00" ]
blueCircle (a, b) =
    circleGem (a, b) [ fill "#0B79CE" ]
greenCircle (a, b) =
    circleGem (a, b) [ fill "#0f0" ]
-}

some (x, y) =
    ngon 6 15
        |> filled (hsla 1.0 0 0 1.0)
        |> move (x, y)

drawContainer =
    collage 600 600

drawShape el ab =
    let
        dbg = Debug.log "asdf" (el, ab)
    in
    case el of
        Just 1 -> some ab
        Just 2 -> some ab
        Just 3 -> some ab
        Just 4 -> some ab
        Just 5 -> some ab
        Just 6 -> some ab
        Nothing -> some ab
        default -> Debug.crash "Unhandled shape tpye!"
