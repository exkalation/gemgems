module GemGems exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)
import Random
import Dict

import Hexagons.Map as HexMap exposing (..)
import Hexagons.Layout as HexL exposing (Orientation, orientationLayoutPointy, Point)
import Hexagons.Hex as Hex exposing (..)


type Gem
    = NoGem
    | RedSquare | BlueSquare | GreenSquare
    | RedCircle | BlueCircle | GreenCircle

gem shape =
    svg [ width "120", height "120", viewBox "0 0 120 120" ] [ shape [] ]
rectGem props =
    rect (List.append [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] props)
circleGem props =
    circle (List.append [ cx "60", cy "60", SvgA.r "50" ] props)
redSquare =
    gem (rectGem [ fill "#f00" ])
blueSquare =
    gem (rectGem [ fill "#0B79CE" ])
greenSquare =
    gem (rectGem [ fill "#0f0" ])
redCircle =
    gem (circleGem [  fill "#f00" ])
blueCircle =
    gem (circleGem [ fill "#0B79CE" ])
greenCircle =
    gem (circleGem [ fill "#0f0" ])

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL

type alias Point =
    ( Float, Float )
type alias Model =
    { dieFace : List (List Int)
    , hexagon: HexMap.Map
    , layout: HexL.Layout
    }


init : (Model, Cmd Msg)
init =
    (Model
        [ [ 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0 ]
        , [ 0, 0, 0, 0, 0, 0 ]
        ]
        (rectangularPointyTopMap 2 2)
        (HexL.Layout
            (orientationLayoutPointy)
            (150, 150)
            (10, 10)
        )
    , roll)



-- UPDATE

type Msg
    = Roll
   | NewFace (List (List Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        --x = Debug.log "update::Model" model
        y = (Dict.map (\x y -> Debug.log "y" (HexL.hexToPoint model.layout y)) model.hexagon)
    in
        case msg of
            Roll ->
                (model, roll)

            NewFace newFace ->
                ({ model | dieFace = newFace }, Cmd.none)

roll =
    Random.generate NewFace (Random.list 6 (Random.list 6 (Random.int 1 6)))


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div []
            (List.map (\x -> (div[] (List.map draw x))) model.dieFace)
            --[ svg [ width "1000", height "1000", viewBox "0 0 1000 1000" ]
            --    (List.map (\x -> myR (HexL.hexToPoint model.layout x)) (Dict.values model.hexagon))
                --[ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15", fill "#f00" ] []
                --, rect [ x "100", y "100", width "100", height "100", rx "15", ry "15", fill "#0f0" ] []
                --]
            --]
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]

myR : Point -> Svg Msg
myR (a, b) =
    let
        ret = rect [ x (toString a), y (toString b), width "100", height "100", rx "15", ry "15", fill "#f00" ] []
        dbg = Debug.log "test" ret
    in
        ret

draw el =
    case el of
        1 -> redSquare
        2 -> blueSquare
        3 -> greenSquare
        4 -> redCircle
        5 -> blueCircle
        6 -> greenCircle
        default -> Html.text (toString el)