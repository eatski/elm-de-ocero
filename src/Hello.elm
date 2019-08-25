module Hello exposing (..)

import Html exposing (table,tr,td,text,div,Html)
import Html.Attributes exposing (..)
import Browser
import Array exposing (Array)
import Html.Events exposing (..)

-- Types
type alias Model = {cells:Cells Color,turn:Turn}
type Turn = BlackTurn | WhiteTurn
type alias Msg = Point
type alias Element = {point:Point,color:Color,puttable:Bool}
type Color = Black | White | None

type alias X = Int
type alias Y = Int
type alias Point = {x:X,y:Y}
type alias Cells a = Array (Array a)
type MoveTo = Up | Down | Right | Left | UpRight | UpLeft | DownRight | DownLeft

-- ShortUtils

select : ((a -> b -> b -> b) -> b) -> a -> b
select test tag = test (\a -> iif (a == tag))

iif : Bool -> a -> a -> a
iif bool th el = if bool then th else el

ls = Array.toList

find : (a -> Bool) -> Array a -> Maybe a
find test ary = 
    let
        f : Int -> Maybe a
        f i = Maybe.andThen (\a -> iif (test a) (Just a) (f <| i + 1)) (Array.get i ary)
    in
        f 0

findAny : (a -> Bool) -> Array a -> Bool
findAny test ary = 
    let
        f : Int -> Bool
        f i = Maybe.withDefault False <| Maybe.map (\a -> iif (test a) True (f <| i + 1)) (Array.get i ary)
    in
        f 0

indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap = Array.indexedMap

createList : Int -> (Int -> a) -> List a
createList len gene = List.map gene (List.range 0 (len - 1))

-- Ocero
setXY : Array Point -> (a -> a) -> Cells a -> Cells a
setXY points fn = indexedMap (\y -> indexedMap (\x -> \e -> iif (findAny ((==) {x=x,y=y}) points) (fn e) e )) 

turnToColor : Turn -> Color
turnToColor turn = case turn of
    BlackTurn -> Black 
    WhiteTurn -> White

getByPoint : Point -> Cells a -> Maybe a
getByPoint point cells  = Maybe.andThen (Array.get point.x) (Array.get point.y cells)

move : MoveTo -> Point -> Point
move moveTo point = 
    let
        toPoint = case moveTo of 
            Up -> {x=0,y=1} 
            Down -> {x=0,y=-1}
            Right -> {x=1,y=0}
            Left -> {x=-1,y=0}
            UpRight -> {x=1,y=1}
            UpLeft -> {x=-1,y=1}
            DownRight -> {x=1,y=-1}
            DownLeft -> {x=-1,y=-1}
     in
        {x=point.x + toPoint.x,y=point.y + toPoint.y}

iteratePoints : Turn -> MoveTo -> Point -> (Point -> b ->  b) -> b -> Cells Color  -> b
iteratePoints turn moveTo point fn initVal cells = 
    let
        andThen : Point -> b -> Color -> Maybe b
        andThen currentPoint current  = 
            select (
                \t 
                    -> t None initVal
                    <| t (turnToColor turn) current
                    <| next (move moveTo currentPoint) (fn currentPoint current )
            ) >> Just
            
        next : Point -> b -> b
        next currentPoint current = 
            Maybe.withDefault initVal <| Maybe.andThen (andThen currentPoint current) (getByPoint currentPoint cells)
    in
    next (move moveTo point) initVal


reversibleDirections : Array MoveTo
reversibleDirections = Array.fromList [
        Up ,Down ,Right, Left ,
        UpRight ,UpLeft ,DownRight ,DownLeft
    ]

-- init

init : () -> (Model,Cmd Msg)
init _ = 
    let 
        create8 : (Int -> a) -> Array a
        create8 = Array.initialize 8 
        put : Point -> Color
        put = select  
                <| \t 
                -> t {x=3,y=4} White 
                <| t {x=4,y=3} White
                <| t {x=4,y=4} Black
                <| t {x=3,y=3} Black None
        createCells = create8 (\y -> create8(\x -> put {x=x,y=y}))
    in
        ({cells=createCells,turn=BlackTurn},Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = (Debug.log (Debug.toString msg) (updateModel msg model),Cmd.none)

updateModel : Msg -> Model -> Model
updateModel msg model = 
    Model 
        (setStone msg model.turn model.cells) 
        <| case model.turn of 
            WhiteTurn -> BlackTurn
            BlackTurn -> WhiteTurn

setStone : Point -> Turn -> Cells Color -> Cells Color
setStone point turn cells = setXY (getReversePoints point turn cells) (\_ -> turnToColor turn) cells

getReversePoints : Point -> Turn -> Cells Color -> Array Point
getReversePoints point turn cells = 
    let
        getReversePointsAtOneDirection : MoveTo -> Array Point
        getReversePointsAtOneDirection moveTo = iteratePoints turn moveTo point Array.push Array.empty cells
    in
    Array.foldl (Array.append << getReversePointsAtOneDirection) (Array.fromList [point]) reversibleDirections

pointToElement : Model -> Point -> Color -> Element 
pointToElement model point color =
    let 
        toElemWithPuttable = Element point color
        isPuttableWhenNone = findAny isReversible reversibleDirections

        isReversible : MoveTo -> Bool
        isReversible moveTo  = 0 < iteratePoints model.turn moveTo point (\_ -> (+) 1) 0 model.cells 
    in
        if color == None then toElemWithPuttable isPuttableWhenNone else toElemWithPuttable False

view : Model -> Html Msg
view model =
    let
        numAttr : String -> Int -> Html.Attribute Msg
        numAttr name value = attribute name (String.fromInt value)
        colorToText color = case color of 
            Black -> "黒"
            White -> "白"
            None -> "" 

        cell : Element -> Html Msg
        cell e =  
            let
                size = "30px"
                circleSize = "90%"
                cellStyle = [
                        style "width" size ,
                        style "height" size,
                        align "center" 
                    ]
                colorToHex  = case e.color of 
                    Black -> "black"
                    White -> "ghostwhite"
                    None -> if e.puttable then "seagreen" else "green"
            in
                td 
                    (iif e.puttable (onClick e.point :: cellStyle) cellStyle)
                    [
                        div [
                            style "width" circleSize ,
                            style "height" circleSize,
                            style "border-radius" "50%",
                            style "background-color" colorToHex
                        ]
                        []
                    ]
        line : Y -> Array Color -> Html Msg
        line y clms = tr [] <| List.indexedMap (\x -> \e -> cell <| pointToElement model (Point x y) e) (ls clms)
        border = numAttr "border"
        bgcolor = attribute "bgcolor"
    in  
        div [] [
            div [] [ model.turn |> turnToColor |> colorToText |> text] ,
            table [border 1, style "border-collapse" "collapse", style "border-color" "black" , style "background-color" "green" ] <| List.indexedMap line <| ls model.cells
        ]

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \ _ -> Sub.none
    , view = view
    }
