module RenderSvg exposing (..)

import Json.Decode as Json exposing (succeed)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (..)
import Gem exposing (..)


base : String
base =
    [ (30.1,84.5), (10.2,50), (30.1,15.5), (69.9,15.5), (89.8,50), (69.9,84.5) ]
    |> List.map (\(a, b) -> (toString a) ++ "," ++ (toString b))
    |> String.join ", "

drawContainer =
    svg [ width "500", height "520", viewBox "0 0 500 520", stroke "#f00" ]

gemToColor gem xs ys =
    case gem of
        Just (Gem.Color x) -> ("type-" ++ (toString x), [])
        Just (Gem.Matched x) -> ("matched type-" ++ (toString x),
            [ animateTransform
                 [ attributeName "transform"
                 , SvgA.type_ "rotate"
                 , from ("0 " ++ ys ++ " " ++ xs)
                 , to ("360 " ++ ys ++ " " ++ xs)
                 , dur "1s", repeatCount "indefinite"
                 ] []
             , animateTransform
                [ attributeName "transform"
                , SvgA.type_ "scale"
                , from "0.91" , to "0.1"
                , begin "0s", dur "1s", repeatCount "1"
                ] []
             ])
        Just Gem.Dragged -> ("type-dragged", [])
        Just Gem.Empty -> ("type-empty", [])
        Nothing -> ("type-empty", [])

drawShape gemType (x, y) msgDown msgUp =
    let
        (color, animations) = gemToColor gemType (toString 50) (toString 50)
    in
        g [ transform ("translate(" ++ (toString x) ++ "," ++ (toString y) ++ ") scale(0.91)") ]
        [ polygon [ class color, SvgA.stroke "#000", onMouseDown msgDown, onMouseUp msgUp, points base ]
            animations
        ]
