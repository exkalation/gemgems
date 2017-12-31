module RenderSvg exposing (..)

import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (..)
import Gem

base : String
base =
    --[ (30.1,84.5), (10.2,50), (30.1,15.5), (69.9,15.5), (89.8,50), (69.9,84.5) ]
    [ (20,70), (0,35), (20,0), (60,0), (80,35), (60,70) ]
    |> List.map (\(a, b) -> (toString a) ++ "," ++ (toString b))
    |> String.join ", "

drawContainer =
    svg [ width "100%", viewBox "0 0 410 540", stroke "#f00" ]

gemToColor gem =
    case gem of
        Just (Gem.Color c) -> (Gem.toString (Gem.Color c), "color type-" ++ (toString c), [])
        Just (Gem.Matched c) -> (Gem.toString (Gem.Matched c), "matched type-" ++ (toString c), [])
        Just Gem.Dragged -> (Gem.toString Gem.Dragged, "type-dragged", [])
        Just (Gem.Moving c y) -> (Gem.toString (Gem.Moving c y), "type-moving color type-" ++ (toString c), [])
        Just Gem.Empty -> (Gem.toString Gem.Empty, "type-empty", [])
        Nothing -> ("Nothing", "type-empty", [])

drawShape gemType (x, y) msgDown msgUp =
    let
        (zz, color, animations) = gemToColor gemType
        yy = case gemType of
            Just (Gem.Moving _ dist) -> y - dist
            default -> y
    in
        g [ id ("id-g-" ++ zz ++ "-" ++ (toString x) ++ "-" ++ (toString yy)), transform ("translate(" ++ (toString x) ++ "," ++ (toString yy) ++ ")") ]
            [ polygon [ id ("id-p-" ++ zz ++ "-" ++ (toString x) ++ "-" ++ (toString yy) ++ "-" ++ color), class color, SvgA.stroke "#000", onMouseDown msgDown, onMouseUp msgUp, points base ]
                animations
            ]
