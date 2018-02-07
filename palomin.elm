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


type Direction = Left | Right | None

type alias Model =
  {
  lastStep : Int,
  lastScreenUpdate : Int, 
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
      lastStep = -1,
      lastScreenUpdate = -1,
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
           generated = Random.generate Initial (RandomList.shuffle (allCells model.size))
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
    let newMs =  Time.inMilliseconds t |> truncate
        modelWithSteps m = if newMs - model.lastStep > 500 then moveAndCheck { m | lastStep = newMs } 
                           else m
        modelWithScreenUpdate m = if newMs - model.lastScreenUpdate > 15000 then makeBombsAndCherries { m | lastScreenUpdate = newMs } 1
                                  else m                        
    in  modelWithScreenUpdate (modelWithSteps model)
    
boom : Model -> Bool
boom model = List.any (\b -> b == model.man) model.bombs

eat : Model -> Bool
eat model = List.any (\b -> b == model.man) model.cherries
           
           
moveY : XY -> Int -> Int
moveY size y = if y <= 0 then size.y - 1
                  else y - 1
                  
                  
moveX : XY -> Int -> Direction -> Int
moveX size x d = case d of
                      Left -> if x <= 0 then size.x - 1
                              else x - 1                        
                      Right -> if x >= size.x - 1 then 0
                         else x + 1
                      None -> x                  
                      
move : Model -> Model
move model =
    let moveMan man = { man | 
            x = moveX model.size model.man.x model.direction, 
            y = moveY model.size model.man.y  
        }
    in { model | man = moveMan model.man, direction = None }

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
        play "http://soundbible.com/grab.php?id=1986&type=mp3",
        text "BOOM!"
        ]
        
eaten : Model -> List (Html Msg)
eaten model = if model.eaten then [play "http://soundbible.com/grab.php?id=2067&type=mp3"]
              else []
      
play : String -> Html Msg
play s = audio [ 
      src s,
      controls False,
      autoplay True
      ] []



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

viewGraphic : Model -> Html Msg
viewGraphic model =
    let sizex = Basics.toFloat model.size.x
        sizey = Basics.toFloat model.size.y
        uiImage src = image (1.0, 1.0) src
        bombImage = uiImage "https://openclipart.org/download/191907/pAK004-boom.svg"
        cherryImage = uiImage "https://openclipart.org/download/183893/simple-apple.svg"
        manImage = uiImage "https://openclipart.org/download/201867/cutecat2.svg"
        uiBombs =  List.map (\xy -> bombImage |> moveXY xy)  model.bombs
        uiCherries =  List.map (\xy -> cherryImage |> moveXY xy)  model.cherries
        uiMan = manImage |> moveXY model.man  
        rect = rectangle  (sizex + 1.0) (sizey + 1.0) |> outlined { defaultLineStyle | thickness = ultrathin } |> shift (-0.5, 0.5)
        oppositeSideY y =  sizey - y     
        centerX x = x - sizex / 2.0
        centerY y = y - sizey / 2.0
        moveXY xy img = move (Basics.toFloat xy.x) (Basics.toFloat xy.y) img 
        move x y img = shift (centerX x, oppositeSideY y |> centerY) img
        uiItems = uiBombs ++ uiCherries ++ [uiMan, rect ] 
    in group uiItems     
        |> scale 20
        |> svg    
    
-- UTIL

makeBombsAndCherries : Model -> Int -> Model
makeBombsAndCherries model n = 
    let addBomb m b =  { m | bombs = m.bombs ++ b }
        addCherry m c =  { m | cherries = m.cherries ++ c }
        makeBombs m = getRandoms m n addBomb  
        makeCherries m = getRandoms m n addCherry 
    in makeBombs model |> makeCherries

getRandoms : Model -> Int -> (Model -> List XY -> Model) -> Model
getRandoms model l changeModel= 
    let randoms = model.randoms
        updated = { model | randoms = drop l randoms }        
    in  changeModel updated (take l randoms)   

allCells : XY -> List XY
allCells size = 
    let values n = List.range 0 (n - 1)
    in List.concatMap (\y -> List.map (\x -> XY x y) (values size.x)) (values size.y)  

