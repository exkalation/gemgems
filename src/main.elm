module GemGems exposing (..)

import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (id)
import Html.Events exposing (..)
import Random
import Dict
import RenderSvg as Render exposing (..)
import Gem exposing (..)

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
    { gems : Dict.Dict HexMap.Hash Gem.Gem
    , grid: HexMap.Map
    , layout: HexL.Layout
    , dragged: Maybe (HexMap.Hash, Hex, Gem.Gem)
    }

init : (Model, Cmd Msg)
init =
    let
        grid = (rectangularPointyTopMap 8 7)
    in
        (Model
            Dict.empty
            grid
            (HexL.Layout (orientationLayoutPointy) (38, 38) (0, 0))
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
                    x = Dict.insert (a, b, c) (Gem.Color newFace) model.gems
                in
                    ({ model | gems = x }, Cmd.none)

            DragStart (k, v) ->
                if model.dragged /= Nothing then (model, Cmd.none)
                else
                    let
                        dbg = Debug.log "DRAG_START" v
                        gem = case (Dict.get k model.gems) of
                            Just x -> x
                            Nothing -> Debug.crash "Not possible: DragStart"
                        m1 = (unhighlight model)
                    in
                        ({ m1 | gems = highlightDraged k m1.gems, dragged = Just (k, v, gem) }, Cmd.none)

            DragEnd (k, v) ->
                if model.dragged == Nothing then (model, Cmd.none)
                else
                    let
                        dbg = Debug.log "DRAG_END" v
                        --ne = neighbor v NE
                        --e = neighbor v E
                        --dbg2 = Debug.log "NEIGHBORS" [ (v, HexMap.hashHex v), (ne, HexMap.hashHex ne), (e, HexMap.hashHex e) ]
                        targetGem = case (Dict.get k model.gems) of
                            Just x -> x
                            Nothing -> Gem.Empty
                        (draggedHash, draggedHex, draggedGem) = getDragged model
                        m1 =
                            if (Hex.distance draggedHex v) == 1 && (draggedGem /= targetGem) then
                                swapGems (k, v) model
                                    |> eliminateFrom (k, v)
                                    |> eliminateFrom (draggedHash, draggedHex)
                                    |> dropIt
                            else unhighlight model
                    in
                        ({ m1 | dragged = Nothing }, Cmd.none)


getBottomLine model =
    getBottom (IntCubeHex (0,0,0)) model

getBottom hex model =
    let
        dbg = Debug.log "going..." hex
        upLeft = Dict.get (neighbor hex SE |> hashHex) model.gems
        up = Dict.get (neighbor hex SW |> hashHex) model.gems
        upRight = Dict.get (neighbor hex W |> hashHex) model.gems
        downRight = Dict.get (neighbor hex NW |> hashHex) model.gems
        down = Dict.get (neighbor hex NE |> hashHex) model.gems
        downLeft = Dict.get (neighbor hex E |> hashHex) model.gems
    in
        if upLeft /= Nothing && down /= Nothing then
            getBottom (neighbor hex SE) model -- go upLeft
        else if upLeft == Nothing && up == Nothing && downLeft /= Nothing then
            getBottom (neighbor hex E) model -- go downLeft
        else if upLeft == Nothing && down /= Nothing && downLeft == Nothing then
            getBottom (neighbor hex NE) model -- go down
        else if up /= Nothing && down == Nothing && downRight /= Nothing then
            hex :: getBottom (neighbor hex NW) model -- go downRight
        else if upLeft /= Nothing && up /= Nothing && down == Nothing && downRight == Nothing then -- = bottom line, lower
            hex :: getBottom (neighbor hex W) model -- go upRight
        else
            [] -- end

dropIt model =
    let
        gem = Debug.log "DROPIT" (neighbor (IntCubeHex (7,0,-7)) NE)
    in
        refillGrid (getBottomLine model) model

refillGrid hexes model =
    case hexes of
        [] -> model
        hex::rest -> refillCol hex model |> refillGrid rest

refillCol colBottomHex model =
    let
        hexUp = getGridUp colBottomHex
        gem = Dict.get (HexMap.hashHex colBottomHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> refillCol hexUp model
            Just (Gem.Empty) -> findDrop colBottomHex hexUp model |> refillCol hexUp
            default -> model -- border reached

findDrop dropToHex checkHex model =
    let
        up = getGridUp checkHex
        dbg = Debug.log "HEREEE0" (dropToHex, checkHex)
        gem = Dict.get (HexMap.hashHex checkHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> Debug.log "HEREEE1" { model | gems = (setGemFromHex dropToHex (Gem.Color c) model.gems |> setGemFromHex checkHex (Gem.Empty)) }
            Just (Gem.Empty) -> findDrop dropToHex up model
            default -> model -- border reached

setGemFromHex hex gem gems =
    Dict.insert (HexMap.hashHex hex) gem gems

getGridUp hex =
    neighbor hex SW

runElimination model =
    Dict.toList model.grid
        |> List.map ((flip eliminateFrom) model)

eliminateFrom (hash, hex) model =
    let
        gem = Debug.log "ELIMINATEFROM" (getExistingGem hash model.gems)
    in
        if Gem.isColor gem then
            scan model gem hex []
                |> eliminateFound model
        else model

eliminateFound model found =
    let
        x = Debug.log "TO BE ELIMIATED" found
    in
        if (List.length found) > 2 then
            { model | gems = model.grid
                |> Dict.map (\hash hex ->
                    if List.member hex found then Gem.Empty
                    else (getExistingGem hash model.gems))
                --|> Dict.fromList
            }
        else model

scan model scanGem fromHex found =
    let
        --x = Debug.log "SCAN" (fromHex, found)
        visited = List.member fromHex found
        fromHash = HexMap.hashHex fromHex
        candidate = (not visited) && case (Dict.get fromHash model.gems) of
            Just x -> x == scanGem
            Nothing -> False -- we reached the border...
        f = if candidate then fromHex :: found else found
    in
        --if List.filter found  |> List.length > 0
        if candidate then f
            |> scan model scanGem (neighbor fromHex NE)
            |> scan model scanGem (neighbor fromHex E)
            |> scan model scanGem (neighbor fromHex SE)
            |> scan model scanGem (neighbor fromHex SW)
            |> scan model scanGem (neighbor fromHex W)
            |> scan model scanGem (neighbor fromHex NW)
        else f

swapGems (hashTarget, _) model =
    let
        (hashDragged, _, gemDragged) = getDragged model
        gemTarget = getExistingGem hashTarget model.gems
        gems1 = model.gems
            |> Dict.insert hashDragged gemTarget
            |> Dict.insert hashTarget gemDragged
    in
        { model | gems = gems1 }

getDragged model =
    case model.dragged of
        Just dragged -> dragged
        Nothing -> Debug.crash "Not possible: getDragged"

--getHex hash model =
--    case model.grid of
--        Just hex -> hex
--        Nothing -> Debug.crash "Not possible: getHex"

getExistingGem hash gems =
    case (Dict.get hash gems) of
       Just gem -> gem
       Nothing -> Debug.crash ("Not possible: getGem " ++ (toString hash) ++ (toString gems))

unhighlight model =
    { model | gems = case model.dragged of
         Just (hash, hex, gem) -> Dict.insert hash gem model.gems
         Nothing -> model.gems, dragged = Nothing
     }

rollAll hexMap =
    hexMap
        |> Dict.keys
        |> List.map rollHex
        |> Cmd.batch

rollHex hash =
    Random.generate (NewFace hash) (Random.int 1 6)

highlightDraged draggedHash gems =
    Dict.insert draggedHash Gem.Dragged gems


-- VIEW

view : Model -> Html Msg
view model =
    let
        dbg = model --Debug.log "VIEW - model.gems" model.gems
    in
        div []
            [ div [ id "board" ]
                [ model.grid
                    |> Dict.map (\k v -> drawShape (Dict.get k model.gems) (HexL.hexToPoint model.layout v) (DragStart (k, v)) (DragEnd (k, v)))
                    |> Dict.values
                    |> drawContainer
                ]
            , button [ onClick Roll ] [ Html.text "Roll" ]
            ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
