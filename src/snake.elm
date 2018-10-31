import Browser
import Browser.Events as Events
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, style)
import Json.Decode as D
import Maybe exposing (withDefault)
import List exposing (head, take, member, tail)
import Process
import Random as R
import Task
import Time

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions: Model -> Sub Msg
subscriptions _ = 
    Sub.batch [
          Events.onKeyPress (D.map toDirection (D.field "key" D.string))
        , Time.every 100 (\_ -> Tick)
    ]

toDirection: String -> Msg
toDirection s =
    case s of
        "q" -> CmdLeft
        "d" -> CmdRight
        "z" -> CmdUp
        "s" -> CmdDown
        _ -> Nope

-- MODEL

type Msg = 
    CmdLeft | CmdRight | CmdUp | CmdDown | 
    GoLeft | GoRight | GoUp | GoDown |
    GenerateTarget Int Int |
    Nope | Tick | CaughtTarget | 
    Halt

type alias Target = { x: Int, y: Int}

type alias Model = 
    { queue : List (Int, Int)
    , target : Target
    , lastDir : Msg
    , size : Int
    , score : Int
    }

type Action = Action Int Int Int Action | EndAction 

initModel : Model
initModel = { queue = [(18, 18), (18, 19), (18, 20), (18, 21)], target = { x = -1, y = -1 }, lastDir = Nope, size = 3, score = 0 }

init : () -> (Model, Cmd Msg)
init _ = (initModel, generateTarget)

-- UPDATE

nextAction : Msg -> Cmd Msg
nextAction action = Task.perform (\_ -> action) (Process.sleep 100)

intGen : R.Generator Int 
intGen = R.int 1 40

generateTarget : Cmd Msg 
generateTarget = R.generate (\(a,b) -> GenerateTarget a b) (R.pair intGen intGen)

updateP : Int -> Int
updateP p = 
    if p < 0 then
        40
    else if p > 40 then
        0
    else
        p

checkTargetIsCaught : Model -> Cmd Msg
checkTargetIsCaught model =
    let (mx, my) = withDefault (1,1) <| head model.queue
    in
        if member (mx, my) (withDefault [] (tail model.queue)) then
            Task.succeed Halt |> Task.perform identity
        else if mx == model.target.x && my == model.target.y then
            Task.succeed CaughtTarget |> Task.perform identity
        else
            Cmd.none

enqueueP : Int -> List (Int, Int) -> (Int, Int) -> List (Int, Int)
enqueueP size q p = p :: (take size q)

moveX : Int -> Model -> Model
moveX v m =
    let (x, y) = withDefault (1, 1) <| head m.queue
        newX = updateP (x + v)
        newQ = enqueueP m.size m.queue (newX, y)
    in { m | queue = newQ }

moveY : Int -> Model -> Model
moveY v m =
    let (x, y) = withDefault (1,1) <| head m.queue
        newY = updateP (y + v)
        newQ = enqueueP m.size m.queue (x, newY)
    in { m | queue = newQ }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CmdLeft     -> 
        case model.lastDir of
            GoRight -> (model, Cmd.none)
            _       -> ({ model | lastDir = GoLeft  }, Cmd.none)
    CmdRight    -> 
        case model.lastDir of
            GoLeft -> (model, Cmd.none)
            _      -> ({ model | lastDir = GoRight }, Cmd.none)
    CmdUp       -> 
        case model.lastDir of
            GoDown -> (model, Cmd.none)
            _      -> ({ model | lastDir = GoUp    }, Cmd.none)
    CmdDown     ->
        case model.lastDir of
            GoUp   -> (model, Cmd.none)
            _      -> ({ model | lastDir = GoDown  }, Cmd.none)

    GoLeft      ->
        (
              moveX -1 model
            , checkTargetIsCaught model
        )
    GoRight     ->
        (
              moveX 1 model
            , checkTargetIsCaught model
        )
    GoUp        ->
        (
              moveY -1 model
            , checkTargetIsCaught model
        )
    GoDown      ->
        (
              moveY 1 model
            , checkTargetIsCaught model
        )

    GenerateTarget x y -> ({ model | target = (Target x y)}, Cmd.none)

    CaughtTarget -> ({ model | size = model.size + 1, score = model.score + 1}, generateTarget)
    
    Nope        -> (model, Cmd.none)

    Tick        -> (model, nextAction (model.lastDir))

    Halt -> (initModel, generateTarget)
-- GRID

divId : Int -> Int -> String
divId x y = String.fromInt x ++ "." ++ String.fromInt y

allDivIds : List (Int, Int)
allDivIds = List.concatMap(\y -> List.map (\x -> (x, y)) (List.range 1 40)) (List.range 1 40)

cellColor : Model -> Int -> Int -> String
cellColor model x y = 
    if List.member (x, y) (model.queue) then "black"
    else if model.target.x == x && model.target.y == y then "red"
    else "gray"

createCells : Model -> List (Int, Int) -> List (Html Msg)
createCells model = 
    List.map (\(x, y) ->
        div [ id (divId x y)
            , style "grid-column" (String.fromInt x)
            , style "grid-row" (String.fromInt y)
            , style "background-color" (cellColor model x y)
        ] []
    )

createGrid : Model -> Html Msg
createGrid model = 
    div [ id "wrapper"
        , style "background-color" "Azure"
        , style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "grid-gap" "5px"
        , style "grid-auto-rows" "minmax(10px, auto)"
        , style "grid-auto-columns" "minmax(10px, auto)"
        , style "float" "left"
    ] (createCells model allDivIds)

createContainer : Model -> Html Msg
createContainer model =
    div [ id "container"
        , style "width" "600px"
        , style "height" "600px"
        , style "float" "left"
    ] [ createGrid model]

scoreDiv : Model -> Html Msg
scoreDiv model = 
    div [ id "score" ] [ 
      text ("Score : " ++ String.fromInt model.score) 
    ]

-- VIEW
view : Model -> Html Msg
view model = 
    div [ style "display" "inline" ] [(createContainer model), (scoreDiv model)]
