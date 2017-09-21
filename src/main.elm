module GemGems exposing (..)

import Debug exposing (log)
import Time exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA exposing (id)
import Html.Events exposing (..)
import Random
import Dict
import RenderSvg as Render exposing (..)
import Gem exposing (..)

import Hexagons.Map as HexMap exposing (..)
import Hexagons.Layout as HexL exposing (Orientation, orientationLayoutPointy, Point)
import Hexagons.Hex as Hex exposing (..)

minMatchLength = 3
numberOfGems = 6

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
    , dirty: Bool
    , bottomLine: List Hex.Hex
    }

init : (Model, Cmd Msg)
init =
    let
        grid = (rectangularPointyTopMap 6 7)
    in
        (Model
            Dict.empty
            grid
            (HexL.Layout (orientationLayoutPointy) (38, 38) (0, 0))
            Nothing
            False
            []
        , rollAll grid)



-- UPDATE

type Msg
    = Roll
   | NewFace HexMap.Hash Int
   | DragStart (HexMap.Hash, Hex)
   | DragEnd (HexMap.Hash, Hex)
   | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model0 =
    let
        model =
            if List.isEmpty model0.bottomLine then {model0 | bottomLine = getBottom (IntCubeHex (0,0,0)) model0.gems }
            else model0
    in
        case msg of
            Roll ->
                ({model | dirty = True }, rollAll model.grid)

            NewFace (a, b, c) newFace ->
                let
                    x = Dict.insert (a, b, c) (Gem.Color newFace) model.gems
                in
                    ({ model | dirty = True, gems = x }, Cmd.none)

            DragStart (k, v) ->
                if model.dirty || model.dragged /= Nothing then (model, Cmd.none)
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
                let
                    m1 = unhighlight model
                in
                case model.dragged of
                    Just (kx, vx, _) ->
                        if model.dragged == Nothing || (kx == k && vx == v) then ({ m1 | dragged = Nothing }, Cmd.none)
                        else
                            let
                                dbg = Debug.log "DRAG_END" v
                                targetGem = case (Dict.get k model.gems) of
                                    Just x -> x
                                    Nothing -> Gem.Empty
                                (draggedHash, draggedHex, draggedGem) = getDragged model
                                m1 =
                                    if (Hex.distance draggedHex v) == 1 && (draggedGem /= targetGem) then
                                        swapGems (k, v) model
                                            |> eliminateFrom (k, v)
                                            |> eliminateFrom (draggedHash, draggedHex)
                                            |> dropIt model.bottomLine
                                    else unhighlight model
                            in
                                ({ m1 | dirty = True, dragged = Nothing }, Cmd.batch <| recolorGrid m1.gems)
                    default -> ({ m1 | dragged = Nothing }, Cmd.none)

            Tick time ->
                if model.dirty then
                    let
                        m1 = runElimination (Dict.toList model.grid) model |> dropIt model.bottomLine
                    in
                        ({ m1 | dirty = False }, Cmd.batch <| recolorGrid m1.gems)
                else (model, Cmd.none)


recolorGrid gems =
    Dict.filter (\hash gem -> isEmpty gem) gems
    |> Dict.keys
    |> List.map rollHex

getBottom : Hex.Hex -> Dict.Dict HexMap.Hash Gem -> List Hex.Hex
getBottom hex gems =
    let
        --dbg = Debug.log "going..." hex
        upLeft = Dict.get (neighbor hex SE |> hashHex) gems
        up = Dict.get (neighbor hex SW |> hashHex) gems
        upRight = Dict.get (neighbor hex W |> hashHex) gems
        downRight = Dict.get (neighbor hex NW |> hashHex) gems
        down = Dict.get (neighbor hex NE |> hashHex) gems
        downLeft = Dict.get (neighbor hex E |> hashHex) gems
    in
        if upLeft /= Nothing && down /= Nothing then
            getBottom (neighbor hex SE) gems -- go upLeft
        else if upLeft == Nothing && up == Nothing && downLeft /= Nothing then
            getBottom (neighbor hex E) gems -- go downLeft
        else if upLeft == Nothing && down /= Nothing && downLeft == Nothing then
            getBottom (neighbor hex NE) gems -- go down
        else if up /= Nothing && down == Nothing && downRight /= Nothing then
            hex :: getBottom (neighbor hex NW) gems -- go downRight
        else if upLeft /= Nothing && up /= Nothing && down == Nothing && downRight == Nothing then -- = bottom line, lower
            hex :: getBottom (neighbor hex W) gems -- go upRight
        else
            [] -- end


dropIt : List Hex.Hex -> Model -> Model
dropIt bottomLine model =
    case bottomLine of
        [] -> model
        hex::rest -> dropInCol hex model |> dropIt rest

dropInCol colBottomHex model =
    let
        hexUp = getGridUp colBottomHex
        gem = Dict.get (HexMap.hashHex colBottomHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> dropInCol hexUp model
            Just (Gem.Empty) -> findDrop colBottomHex hexUp model |> dropInCol hexUp
            default -> model -- border reached

findDrop dropToHex checkHex model =
    let
        up = getGridUp checkHex
        --dbg = Debug.log "HEREEE0" (dropToHex, checkHex)
        gem = Dict.get (HexMap.hashHex checkHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> { model | gems = (setGemFromHex dropToHex (Gem.Color c) model.gems |> setGemFromHex checkHex (Gem.Empty)) }
            Just (Gem.Empty) -> findDrop dropToHex up model
            default -> model -- border reached

setGemFromHex hex gem gems =
    Dict.insert (HexMap.hashHex hex) gem gems

getGridUp hex =
    neighbor hex SW

runElimination hexes model =
    case hexes of
        [] -> model
        (hash, hex)::rest -> eliminateFrom (hash, hex) model |> runElimination rest

eliminateFrom (hash, hex) model =
    let
        dbg = Debug.log "ELIMINATEFROM" hex
        gem = (getExistingGem hash model.gems)
    in
        if Gem.isColor gem then
            scan model gem hex []
                |> eliminateFound model
        else model

eliminateFound model found =
    let
        x = Debug.log "TO BE ELIMIATED" found
    in
        if (List.length found) >= minMatchLength then
            { model | gems = model.grid
                |> Dict.map (\hash hex ->
                    if List.member hex found then (Debug.log "EMPTY IT" Gem.Empty)
                    else (getExistingGem hash model.gems))
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
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" NE))
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" E))
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" SE))
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" SW))
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" W))
            |> scan model scanGem (neighbor fromHex (Debug.log "SCANDIR" NW))
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
    Random.generate (NewFace hash) (Random.int 1 numberOfGems)

highlightDraged draggedHash gems =
    Dict.insert draggedHash Gem.Dragged gems


-- VIEW

view : Model -> Html Msg
view model =
    let
        dbg = model --Debug.log "VIEW - model.gems" model.gems
    in
        div [ HtmlA.style [("margin-left", "500px"), ("border", "1px solid red")] ]
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
  Time.every (millisecond*500) Tick