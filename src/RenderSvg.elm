module RenderSvg exposing (..)

import Html.Attributes as HtmlA exposing (attribute)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Svg.Events as SvgE exposing (..)
import Pointer
import Gem

base : String
base =
    [ (20,70), (0,35), (20,0), (60,0), (80,35), (60,70) ]
    |> List.map (\(a, b) -> (toString a) ++ "," ++ (toString b))
    |> String.join ", "

drawContainer =
    svg [ width "100%", viewBox "0 0 410 540", stroke "#f00" ]

gemToColor gem =
    case gem of
        Just (Gem.Color c) -> "color type-" ++ (toString c)
        Just (Gem.Matched c) -> "matched type-" ++ (toString c)
        Just (Gem.Moving c y) -> "type-moving color type-" ++ (toString c)
        Just Gem.Dragged -> "type-dragged"
        Just Gem.Empty -> "type-empty"
        Nothing -> "type-empty"

drawShape gemType (x, y) msgDown msgUp =
    let
        gs = Maybe.withDefault Gem.Empty gemType |> Gem.toString
        color = gemToColor gemType
        y_ = case gemType of
            Just (Gem.Moving _ dist) -> y - dist
            default -> y
        (xs, ys) = (toString x, toString y_)
        gids = "id-g-" ++ gs ++ "-" ++ xs ++ "-" ++ ys
        pids = gids ++ color
    in
        g [ id gids, transform ("translate(" ++ xs ++ "," ++ ys ++ ")") ]
        [ polygon
            [ id pids
                , class color
                , SvgA.stroke "#000"
                , Pointer.onDown msgDown
                --, SvgE.onMouseUp msgUp
                , Pointer.onUp msgUp
                , HtmlA.attribute "elm-pep" "true"
                , points base ]
            []
        ]