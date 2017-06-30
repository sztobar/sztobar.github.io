port module Board exposing (..)

import Html exposing (Html, button, div, span, text, br, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (rel, type_, href, class)
import Dict exposing (Dict, fromList)
import Time
import Platform.Sub

type Symbol
    = Cross
    | Star
    | Brackets
    | Shield
    | Heart
    | Moon
    | Arrow
    | Thunder
    | Moustache
    | Plus
    | Check
    | Cloud

type alias Tile =
    { shown : Bool
    , symbol : Symbol }

type State
  = StartScreen
  | Preview
  | Board
  | GameOver

type alias Model =
    { tiles : Dict (Int, Int) Tile
    , tempShown1 : Maybe (Int, Int)
    , tempShown2 : Maybe (Int, Int)
    , state : State
    , time : Int
    }

init : (Model, Cmd Msg)
init =
    ({ tiles = fromList
      [ ((0, 0), { shown = False, symbol = Cross }), ((0, 1), { shown = False, symbol = Cross })
      , ((0, 2), { shown = False, symbol = Star }), ((0, 3), { shown = False, symbol = Star })
      , ((1, 0), { shown = False, symbol = Brackets }), ((1, 1), { shown = False, symbol = Brackets })
      , ((1, 2), { shown = False, symbol = Shield }), ((1, 3), { shown = False, symbol = Shield })
      , ((2, 0), { shown = False, symbol = Heart }), ((2, 1), { shown = False, symbol = Heart })
      , ((2, 2), { shown = False, symbol = Moon }), ((2, 3), { shown = False, symbol = Moon })
      , ((3, 0), { shown = False, symbol = Arrow }), ((3, 1), { shown = False, symbol = Arrow })
      , ((3, 2), { shown = False, symbol = Thunder }), ((3, 3), { shown = False, symbol = Thunder })
      , ((4, 0), { shown = False, symbol = Moustache }), ((4, 1), { shown = False, symbol = Moustache })
      , ((4, 2), { shown = False, symbol = Plus }), ((4, 3), { shown = False, symbol = Plus })
      , ((5, 0), { shown = False, symbol = Check }), ((5, 1), { shown = False, symbol = Check })
      , ((5, 2), { shown = False, symbol = Cloud }), ((5, 3), { shown = False, symbol = Cloud })
      ]
    , tempShown1 = Nothing
    , tempShown2 = Nothing
    , state = StartScreen
    , time = 0
    }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Preview ->
      Time.every Time.second Tick
    Board ->
      Time.every Time.second Tick
    _ ->
      newBoard NewBoard

-- PORTS

port shuffleBoard : Int -> Cmd msg

port newBoard: (List (Int, Int) -> msg) -> Sub msg

-- MAIN

main =
  Html.program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type Msg
  = Click (Int, Int)
  | StartGame
  | NewBoard (List (Int, Int))
  | Tick Time.Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click pos ->
      (clickTile pos model, Cmd.none)
    StartGame ->
      (model, shuffleBoard 1)
    NewBoard source ->
      ({ model
         | tiles = createBoard source
         , time = 10
         , state = Preview
       }, Cmd.none)
    Tick time ->
      (tickTime model, Cmd.none)

tickTime : Model -> Model
tickTime model =
  case model.state of
    Preview ->
      if model.time > 1 then
        { model | time = model.time - 1 }
      else
        { model
          | time = 0
          , state = Board
          , tiles = hideAllTiles model.tiles
        }
    Board ->
      { model | time = model.time + 1 }
    _ -> model

createBoard : List (Int, Int) -> Dict (Int, Int) Tile
createBoard source =
  source
    |> List.indexedMap (\idx pos ->
         (pos, tileFromIdx idx)
       )
    |> Dict.fromList

newTile : Symbol -> Tile
newTile symbol =
  { symbol = symbol, shown = True }

tileFromIdx : Int -> Tile
tileFromIdx idx =
  if idx < 2 then
    newTile Cross
  else if idx < 4 then
    newTile Star
  else if idx < 6 then
    newTile Brackets
  else if idx < 8 then
    newTile Shield
  else if idx < 10 then
    newTile Heart
  else if idx < 12 then
    newTile Moon
  else if idx < 14 then
    newTile Arrow
  else if idx < 16 then
    newTile Thunder
  else if idx < 18 then
    newTile Moustache
  else if idx < 20 then
    newTile Plus
  else if idx < 22 then
    newTile Check
  else
    newTile Cloud

clickTile : (Int, Int) -> Model -> Model
clickTile pos model =
  case model.tempShown1 of
    Nothing ->
      { model
        | tempShown1 = Just pos
        , tiles = showTile pos model.tiles
      }
    Just tempPos1 ->
      case model.tempShown2 of
        Nothing ->
          if (isSameSymbol tempPos1 pos model.tiles) then
            { model
              | tempShown1 = Nothing
              , tiles = showTile pos model.tiles
            }
          else
            { model
              | tempShown2 = Just pos
              , tiles = showTile pos model.tiles
            }
        Just tempPos2 ->
          { model
            | tempShown1 = Just pos
            , tempShown2 = Nothing
            , tiles = model.tiles
                |> hideTile tempPos1
                |> hideTile tempPos2
                |> showTile pos
          }

isSameSymbol : (Int, Int) -> (Int, Int) -> Dict (Int, Int) Tile -> Bool
isSameSymbol pos1 pos2 tiles =
  let
    m1 = Dict.get pos1 tiles
    m2 = Dict.get pos2 tiles
    result = Maybe.map2 (\tile1 tile2 ->
      tile1.symbol == tile2.symbol
    ) m1 m2
  in
    Maybe.withDefault False result

showTile : (Int, Int) -> Dict (Int, Int) Tile -> Dict (Int, Int) Tile
showTile pos tiles =
  Dict.update pos (\m ->
    --Maybe.map (\tile -> { tile | shown = True }) m
    case m of
      Just tile ->
        Just { tile | shown = True }
      Nothing ->
        Nothing
  ) tiles

hideTile : (Int, Int) -> Dict (Int, Int) Tile -> Dict (Int, Int) Tile
hideTile pos tiles =
  Dict.update pos (\m ->
    case m of
      Just tile ->
        Just { tile | shown = False }
      Nothing ->
        Nothing
  ) tiles

hideAllTiles : Dict (Int, Int) Tile -> Dict (Int, Int) Tile
hideAllTiles tiles =
  Dict.map (\pos tile ->
      { tile | shown = False }
  ) tiles

view : Model -> Html Msg
view model =
  div []
    [ div [ class "timer" ] [ text (toString model.time) ]
    , div [ class "container" ] (List.map (\y ->
      div [ class "row" ]
        (List.concat
          [ List.map (\x ->
              viewTile (y, x) model.tiles
            ) [0, 1, 2, 3]
          , [ br [] [] ]
          ])
    ) [0, 1, 2, 3, 4, 5])
    , div [ class "overlay" ] [ viewState model ]]


viewTile : (Int, Int) -> Dict (Int, Int) Tile -> Html Msg
viewTile pos tiles =
  let
    m = Dict.get pos tiles
  in
    case m of
      Just tile ->
        div [ class ("tile " ++ getTileClassName tile)
            , onClick (Click pos) ]
            []
      Nothing ->
        div [] []

getTileClassName : Tile -> String
getTileClassName tile =
  case tile.shown of
    True ->
      case tile.symbol of
        Cross -> "tile__cross"
        Star -> "tile__star"
        Brackets -> "tile__brackets"
        Shield -> "tile__shield"
        Heart -> "tile__heart"
        Moon -> "tile__moon"
        Arrow -> "tile__arrow"
        Thunder -> "tile__thunder"
        Moustache -> "tile__moustache"
        Plus -> "tile__plus"
        Check -> "tile__check"
        Cloud -> "tile__cloud"
    False -> "tile__hide"

viewState : Model -> Html Msg
viewState model =
  case model.state of
    StartScreen -> viewStartScreen model
    Board -> div [] []
    Preview -> div [] []
    GameOver -> viewGameOverScreen model

viewStartScreen : Model -> Html Msg
viewStartScreen model =
  div []
      [ div [ class "header" ] [ text "Welcome to memonsters game" ]
      , div [ class "button"
            , onClick StartGame
            ]
        [ text "Start game" ]
      ]

viewGameOverScreen : Model -> Html Msg
viewGameOverScreen model =
  div []
      [ div [ class "header" ] [ text "Game Over" ]
      , div [ class "subheader" ] [ text "Your time is: "]
      , div [ class "subheader__time" ] [ text (toString model.time) ]
      , div [ class "button"
            , onClick StartGame
            ]
        [ text "Start new game" ]
      ]
