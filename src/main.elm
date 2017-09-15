module GemGems exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events exposing (..)
import Random
import Dict
import RenderSvg as Render exposing (..)

import Hexagons.Map as HexMap exposing (..)
import Hexagons.Layout as HexL exposing (Orientation, orientationLayoutPointy, Point)
import Hexagons.Hex as Hex exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model =
    { gems : Dict.Dict HexMap.Hash Int
    , grid: HexMap.Map
    , layout: HexL.Layout
    , dragged: Maybe (HexMap.Hash, Int)
    }

init : (Model, Cmd Msg)
init =
    let
        grid = (rectangularPointyTopMap 8 7)
    in
        (Model
            Dict.empty
            grid
            (HexL.Layout (orientationLayoutPointy) (39, 39) (20, 20))
            Nothing
        , rollAll grid)



-- UPDATE

type Msg
    = Roll
   | NewFace HexMap.Hash Int
   | DragStart (HexMap.Hash, Hex)
   | DragEnd (HexMap.Hash, Hex)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        --x = Debug.log "update::Model" model
        --y = (Dict.map (\x y -> Debug.log "y" (HexL.hexToPoint model.layout y)) model.grid)
        x = 1
    in
        case msg of
            Roll ->
                (model, rollAll model.grid)

            NewFace (a, b, c) newFace ->
                let
                    x = Dict.insert (a, b, c) newFace model.gems
                in
                    ({ model | gems = x }, Cmd.none)

            DragStart (k, v) ->
                let
                    currentValue = case (Dict.get k model.gems) of
                        Just x -> x
                        Nothing -> Debug.crash "Not possible..."
                    unhighlightedGems = unhighlightDraged model.dragged model.gems
                in
                    ({ model | gems = highlightDraged k unhighlightedGems, dragged = Just (k, currentValue) }, Cmd.none)

            DragEnd (k, v) ->
                let
                    dbg = Debug.log "CLICK" (k, v)
                    ne = neighbor v NE
                    e = neighbor v E
                    dbg2 = Debug.log "NEIGHBORS" [ (v, HexMap.hashHex v), (ne, HexMap.hashHex ne), (e, HexMap.hashHex e) ]
                in
                    ({ model | gems = unhighlightDraged model.dragged model.gems, dragged = Nothing }, Cmd.none)

rollAll hexMap =
    hexMap
        |> Dict.keys
        |> List.map (\x -> Random.generate (NewFace x) (Random.int 1 4))
        |> Cmd.batch

highlightDraged dragged gems =
    Dict.insert dragged 9 gems

unhighlightDraged : Maybe (HexMap.Hash, Int) -> Dict.Dict HexMap.Hash Int -> Dict.Dict HexMap.Hash Int
unhighlightDraged dragged gems =
    case dragged of
        Just (hash, gemType) -> Dict.insert hash gemType gems
        Nothing -> gems

-- VIEW

view : Model -> Html Msg
view model =
    let
        dbg = Debug.log "VIEW - model.gems" model.gems
    in
        div []
            [ div [ id "board" ]
                [ model.grid
                    |> Dict.map (\k v -> Render.drawShape (Dict.get k model.gems) (HexL.hexToPoint model.layout v) (DragStart (k, v)) (DragEnd (k, v)))
                    |> Dict.values
                    |> Render.drawContainer
                ]
            , button [ onClick Roll ] [ Html.text "Roll" ]
            ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
