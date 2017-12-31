module GemGems exposing (..)

import Time exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlA exposing (id)
import Html.Events exposing (..)
import Random
import Dict

import Pointer
import Hexagons.Map as HexMap
import Hexagons.Layout as HexL
import Hexagons.Hex as Hex exposing (Direction(..))

import Config
import RenderSvg as Render exposing (..)
import Gem
import Grid

lengthToScore len =
    if len <= Config.minMatchLength then len
    else len + (len - Config.minMatchLength) * 5

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
    , dragged: Maybe (Hex.Hex, Gem.Gem)
    , dirty: Bool -- the model is dirty if gems can be matched, or the grid has matched/empty places
    , tickCounter: Int
    , bottomLine: List Hex.Hex
    , score: Int
    }

init : (Model, Cmd Msg)
init =
    let
        grid = (HexMap.rectangularPointyTopMap 5 5) --6 7
    in
        (Model
            Dict.empty
            grid
            (HexL.Layout (HexL.orientationLayoutPointy) (42, 42) (5, 5))
            Nothing
            False
            0
            []
            0
        , rollAll grid)


-- UPDATE

type Msg
    = Roll
   | NewFace HexMap.Hash Int
   | DragStart Hex.Hex Pointer.Event
   | DragEnd Hex.Hex Pointer.Event
   | Tick Time
   | Nop Pointer.Event


update : Msg -> Model -> (Model, Cmd Msg)
update msg model0 =
    let
        x = Debug.log "action"
        model =
            if List.isEmpty model0.bottomLine then { model0 | bottomLine = Grid.buildBottomLine (Hex.IntCubeHex (0,0,0)) model0.gems }
            else model0
    in
        case msg of
            Roll ->
                ({ model | dirty = True }, rollAll model.grid)

            NewFace (a, b, c) newFace ->
                ({ model | dirty = True, gems = Dict.insert (a, b, c) (Gem.Color newFace) model.gems }, Cmd.none)

            DragStart hex ev ->
                if model.dirty || model.dragged /= Nothing then
                    (model, Cmd.none)
                else
                    handleDragStart hex model

            DragEnd hex ev ->
                let
                    (_, _, hexT) = Debug.log "XXXXXXXXX" (ev, hex, Grid.getHexAt model.layout ev.pointer.offsetPos)
                in
                handleDragEnd hexT model

            Tick time ->
                if model.tickCounter < Config.moveTicks then
                    ({ model | tickCounter = model.tickCounter+1, gems = (moveMovingGems model.gems) }, Cmd.none)
                else if model.dirty then
                    let
                        m1 = markMatchedEmpty model -- do it backwards, to only do one step per tick
                        m2 = if m1.gems == model.gems then dropIt model.bottomLine model else m1
                        cmds = if m2.gems == model.gems then recolorGrid model.gems else []
                        m3 = if m2.gems == model.gems && List.isEmpty cmds then runElimination (Dict.toList model.grid) model else m2
                    in
                        ({ m3 | dirty = isDirty model, tickCounter = 0 }, Cmd.batch cmds)
                else ({ model | tickCounter = 0 }, Cmd.none)

            Nop x ->
                (model, Cmd.none)

moveMovingGems gems =
    Dict.map moveMovingGem gems

moveMovingGem _ gem =
    case gem of
        Gem.Moving c dist ->
            let
                newDist = reduceDistance dist
            in
                if newDist == 0 then Gem.Color c else Gem.Moving c newDist
        default -> gem

reduceDistance dist =
    let
        dx = dist / Config.reduceDistanceFactor
    in
        if dx <= Config.reduceDistanceAbsolute then 0 else dx - Config.reduceDistanceAbsolute

handleDragStart hex model =
    let
        dbg = Debug.log "DRAG_START" hex
        gem = case (Dict.get (HexMap.hashHex hex) model.gems) of
            Just x -> x
            Nothing -> Debug.crash "Not possible: DragStart"
        m1 = (unhighlight model)
    in
        ({ m1 | gems = highlightDragged hex m1.gems, dragged = Just (hex, gem) }, Cmd.none)

handleDragEnd hex model =
    let
        m1 = unhighlight model
    in
        case model.dragged of
            Just (draggedHex, draggedGem) ->
                if draggedHex == hex then
                    ({ m1 | dragged = Nothing }, Cmd.none)
                else
                    let
                        dbg = Debug.log "DRAG_END" hex
                        targetGem = case (Dict.get (HexMap.hashHex hex) model.gems) of
                            Just x -> x
                            Nothing -> Gem.Empty
                        m1 =
                            if (Hex.distance draggedHex hex) == 1 && (draggedGem /= targetGem) then
                                swapGems draggedGem targetGem draggedHex hex model
                            else unhighlight model
                    in
                        ({ m1 | dirty = True, dragged = Nothing }, Cmd.none)
            default -> ({ m1 | dragged = Nothing }, Cmd.none)

markMatchedEmpty model =
    { model | gems = (Dict.map (\_ gem -> case gem of
        Gem.Matched _ -> Gem.Empty
        default -> gem) model.gems)}

isDirty model =
    (Dict.values model.gems
    |> List.any Gem.isEmpty) || model /= (runElimination (Dict.toList model.grid) model)

recolorGrid gems =
    Dict.filter (\hash gem -> Gem.isEmpty gem) gems
    |> Dict.keys
    |> List.map rollHex

dropIt : List Hex.Hex -> Model -> Model
dropIt bottomLine model =
    case bottomLine of
        [] -> model
        hex::rest -> dropInCol hex model |> dropIt rest

dropInCol colBottomHex model =
    let
        hexUp = hexNeighborUp colBottomHex
        gem = Dict.get (HexMap.hashHex colBottomHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> dropInCol hexUp model
            Just (Gem.Empty) -> findDrop colBottomHex hexUp model |> dropInCol hexUp
            default -> model -- matched or border reached

findDrop dropToHex checkHex model =
    let
        up = hexNeighborUp checkHex
        gem = Dict.get (HexMap.hashHex checkHex) model.gems
    in
        case gem of
            Just (Gem.Color c) -> { model | gems = (setGemAtHex dropToHex (Gem.Moving c (Grid.getDiffY model.layout checkHex dropToHex)) model.gems |> setGemAtHex checkHex (Gem.Empty)) }
            Just (Gem.Empty) -> findDrop dropToHex up model
            default -> model -- matched or border reached

setGemAtHex hex gem gems =
    Dict.insert (HexMap.hashHex hex) gem gems

hexNeighborUp hex =
    Hex.neighbor hex SW

runElimination hexes model =
    case hexes of
        [] -> model
        (hash, hex)::rest -> eliminateFrom (hash, hex) model |> runElimination rest

eliminateFrom (hash, hex) model =
    let
        gem = (getExistingGem hex model.gems)
    in
        if Gem.isColor gem then
            scan model gem hex []
                |> eliminateFound model
        else model

eliminateFound model found =
    if (List.length found) >= Config.minMatchLength then
        { model | gems = model.grid
            |> Dict.map (\hash hex ->
                let
                    gem = getExistingGem hex model.gems
                in
                    if List.member hex found then
                        Gem.colorToMatched gem
                    else gem
            )
        }
    else model

scan : Model -> Gem.Gem -> Hex.Hex -> List Hex.Hex -> List Hex.Hex
scan model scanGem fromHex found =
    let
        visited = List.member fromHex found
        fromHash = HexMap.hashHex fromHex
        candidate = (not visited) && case (Dict.get fromHash model.gems) of
            Just x -> x == scanGem
            Nothing -> False -- we reached the border...
        f = if candidate then fromHex :: found else found
    in
        if candidate then f
            |> scan model scanGem (Hex.neighbor fromHex NE)
            |> scan model scanGem (Hex.neighbor fromHex E)
            |> scan model scanGem (Hex.neighbor fromHex SE)
            |> scan model scanGem (Hex.neighbor fromHex SW)
            |> scan model scanGem (Hex.neighbor fromHex W)
            |> scan model scanGem (Hex.neighbor fromHex NW)
        else f

swapGems gemDragged gemTarget hexDragged hexTarget model =
    { model | gems = model.gems
         |> Dict.insert (HexMap.hashHex hexDragged) gemTarget
         |> Dict.insert (HexMap.hashHex hexTarget) gemDragged }

getExistingGem hex gems =
    case (Dict.get (HexMap.hashHex hex) gems) of
       Just gem -> gem
       Nothing -> Debug.crash ("Not possible: getGem " ++ (toString hex) ++ (toString gems))

highlightDragged hex gems =
    Dict.insert (HexMap.hashHex hex) Gem.Dragged gems

unhighlight model =
    { model | gems = case model.dragged of
         Just (hex, gem) -> Dict.insert (HexMap.hashHex hex) gem model.gems
         Nothing -> model.gems, dragged = Nothing
     }

rollAll hexMap =
    hexMap
        |> Dict.keys
        |> List.map rollHex
        |> Cmd.batch

rollHex hash =
    Random.generate (NewFace hash) (Random.int 1 Config.numberOfGems)


-- VIEW

view : Model -> Html Msg
view model =
    let prefix = "" in
    div [ HtmlA.id "game"
        , HtmlA.align "center"
        , HtmlA.classList [ ("game", True), ("working", model.dirty), ("play", not model.dirty) ]
        , Pointer.onDown Nop
        , Pointer.onMove Nop
        , Pointer.onUp Nop
        , HtmlA.attribute "elm-pep" "true"
        ]
        [ Html.node "link" [ HtmlA.rel "stylesheet", HtmlA.href (prefix ++ "/res/styles.css") ] []
        , div [ id "board", HtmlA.align "center" ]
            [ model.grid
                |> Dict.map (\hash hex -> drawShape (Dict.get hash model.gems) (Grid.drawPosition model.layout hex) (DragStart hex) (DragEnd hex))
                |> Dict.values
                |> drawContainer
            ]
        , h3 [ HtmlA.align "center" ]
            [ Html.text (if model.dirty then "Working... please wait..." else "Play!") ]
        , button [ onClick Roll ] [ Html.text "Reroll gems!" ]
        , p [ HtmlA.align "center" ]
            [ Html.text "Match four gems by switching two neighbouring gems (drag and drop)." ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (millisecond * Config.tickDelayMs) Tick