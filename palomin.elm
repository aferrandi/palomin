import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)
import List exposing (..)
import Set exposing (..)
import String exposing (..)
import Keyboard exposing (..)
import Json.Decode as Json
import AnimationFrame exposing (..)
import Time exposing (..)
import Html.Events exposing (on, keyCode)
import Random.List as RandomList 
import Collage exposing (..)
import Collage.Render exposing (svg)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias XY = { x : Int, y : Int }

type alias XYFloat = { x : Float, y : Float }


type Direction = Left | Right | None

type alias Model =
  {
  lastStep : Float,
  lastScreenUpdate : Float, 
  size : XY,
  man : XY,  
  bombs : List XY, 
  cherries : List XY,
  direction : Direction,
  points : Int,
  exploded : Bool,
  eaten : Bool,
  randoms : List XY
  }
  

initialModel : Model
initialModel = {
      lastStep = -1.0,
      lastScreenUpdate = -1.0,
      size =  { x = 40, y = 25 } ,
      man = { x = 20, y= 24 }, 
      bombs = [],
      cherries = [],
      direction = None,
      points = 0,
      exploded = False,
      eaten = False,
      randoms = []
    }


init : (Model, Cmd Msg)
init = let model = initialModel 
           generated = Random.generate Initial (RandomList.shuffle (rangeCells model.size))
       in (model, generated)
    

-- UPDATE

type Msg
  = Initial (List XY) |
    KeyDown Int |
    Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initial randoms -> (handleInitial model randoms, Cmd.none) 
    KeyDown key -> (handleKeyboard model key,  Cmd.none)
    Tick time -> ( handleTick model time, Cmd.none) 

handleKeyboard : Model -> Int -> Model
handleKeyboard model key  = case key of 
                            37 -> { model | direction = Left }
                            39 -> { model | direction = Right}
                            _ -> model

handleInitial : Model -> List XY -> Model
handleInitial model allRandoms = 
    let noDups = List.filter (\xy -> xy /= model.man)
        updated  = { model | randoms = noDups allRandoms }        
    in makeBombsAndCherries updated 20

handleTick : Model -> Time ->  Model
handleTick model t = 
    let newMs =  Time.inMilliseconds t
        modelWithSteps m = if newMs - model.lastStep > 500.0 then moveAndCheck { m | lastStep = newMs } 
                           else m
        modelWithScreenUpdate m = if newMs - model.lastScreenUpdate > 15000.0 then makeBombsAndCherries { m | lastScreenUpdate = newMs } 1
                                  else m                        
    in  modelWithScreenUpdate (modelWithSteps model)
    
boom : Model -> Bool
boom model = List.any (\b -> b == model.man) model.bombs

eat : Model -> Bool
eat model = List.any (\b -> b == model.man) model.cherries
           
           
moveY : Int -> Int -> Int
moveY sizeY y = if y <= 0 then sizeY - 1
                  else y - 1
                  
                  
moveX : Int -> Int -> Direction -> Int
moveX sizeX x d = case d of
                      Left -> if x <= 0 then sizeX - 1
                              else x - 1                        
                      Right -> if x >= sizeX - 1 then 0
                         else x + 1
                      None -> x                  
                      
move : Model -> Model
move model =
    let moveMan man = { 
            x = moveX model.size.x man.x model.direction, 
            y = moveY model.size.y man.y  
        }
    in { model | man = moveMan model.man , direction = None }

moveAndCheck : Model -> Model
moveAndCheck model = 
    let moved = move model
    in if boom moved then { moved | exploded = True }
       else if eat moved then { moved | 
           eaten = True, 
           points = moved.points + 1, 
           cherries = List.filter (\b -> b /= moved.man) moved.cherries 
           } 
       else { moved | eaten = False }

makeBombsAndCherries : Model -> Int -> Model
makeBombsAndCherries model n = 
    let addBomb m b =  { m | bombs = m.bombs ++ b }
        addCherry m c =  { m | cherries = m.cherries ++ c }
        makeBombs m = extractNRandoms m n addBomb  
        makeCherries m = extractNRandoms m n addCherry 
    in makeBombs model |> makeCherries


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
                          AnimationFrame.times Tick,
                          downs KeyDown
                          ]

-- VIEW

view : Model -> Html Msg
view model =
  let textPoints = Html.text ("Points:" ++ toString model.points) 
      fonConsolas = style [ ("font-family", "Consolas") ]      
  in div [fonConsolas] (
      if model.exploded /= True then [viewGraphic model, textPoints] ++  (eaten model)
      else [exploded model]
    )

exploded : Model -> Html Msg
exploded model = div[] [
        playSound "http://soundbible.com/grab.php?id=1986&type=mp3",
        text "BOOM!"
        ]
        
eaten : Model -> List (Html Msg)
eaten model = if model.eaten then [playSound "http://soundbible.com/grab.php?id=2067&type=mp3"]
              else []
      
-- TEXTVIEW 

viewAsText : Model -> Html Msg
viewAsText model = 
   let texts = List.map (\s -> String.fromList s |> Html.text)  (viewInChars model)
       asLines htmls =  List.intersperse (Html.br [] []) htmls 
   in div [] (asLines texts)

viewInChars : Model -> List (List Char)
viewInChars model = 
    let ys = range 0 (model.size.y-1) 
        xs = range 0 (model.size.x-1) 
        row y xs = List.map (\x -> displayCell model { x= x, y = y}) xs
    in List.map (\y -> row y xs) ys

displayCell : Model -> XY -> Char
displayCell model xy = if model.man == xy then 'O'
          else if List.member xy model.bombs then '*'
          else if List.member xy model.cherries then '+'
          else '_'

-- GRAPHICVIEW

xyToFloat : XY -> XYFloat 
xyToFloat xy = { x = Basics.toFloat xy.x,  y = Basics.toFloat xy.y }

centerXY : XYFloat -> XYFloat -> XYFloat
centerXY size xy =  let center sizeV v = v - sizeV / 2.0 
                    in { x = center size.x xy.x, y = center size.y xy.y  }
                    
xyToTuple : XYFloat -> (Float, Float)
xyToTuple xy = (xy.x, xy.y)

moveXY : XYFloat -> Collage msg -> XYFloat ->  Collage msg
moveXY size img xy = let oppositeSideY xy =  { x = xy.x, y = size.y - xy.y }     
                         center xy  = centerXY size xy
                     in shift (oppositeSideY xy |> center |> xyToTuple) img

border : XYFloat -> Collage msg
border size = let toBorder r = outlined { defaultLineStyle | thickness = ultrathin } r
              in rectangle (size.x + 1.0) (size.y + 1.0) |> toBorder |> shift (-0.5, 0.5)

viewGraphic : Model -> Html Msg
viewGraphic model =
    let size = xyToFloat model.size
        uiImage src = image (1.0, 1.0) src
        bombImage = uiImage "https://openclipart.org/download/191907/pAK004-boom.svg"
        cherryImage = uiImage "https://openclipart.org/download/183893/simple-apple.svg"
        manImage = uiImage "https://openclipart.org/download/201867/cutecat2.svg"
        moveImg img xy  = moveXY size img (xyToFloat xy)  
        uiBombs =  List.map (moveImg bombImage)  model.bombs
        uiCherries =  List.map (moveImg cherryImage)  model.cherries
        uiMan = moveImg manImage model.man        
        uiItems = uiBombs ++ uiCherries ++ [uiMan, border size] 
    in group uiItems     
        |> scale 20
        |> svg    

    
-- UTIL

extractNRandoms : Model -> Int -> (Model -> List XY -> Model) -> Model
extractNRandoms model l changeModel= 
    let randoms = model.randoms
        updated = { model | randoms = drop l randoms }        
    in  changeModel updated (take l randoms)   

playSound : String -> Html Msg
playSound s = audio [ 
      src s,
      controls False,
      autoplay True
      ] []


rangeCells : XY -> List XY
rangeCells size = 
    let values n = List.range 0 (n - 1)
    in List.concatMap (\y -> List.map (\x -> XY x y) (values size.x)) (values size.y)  

