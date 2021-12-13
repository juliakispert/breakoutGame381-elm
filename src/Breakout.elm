--
-- The classic game of Asteroids, minus the pesky problem of dying.
--
-- Author: Paul Cantrell
-- documentation, comments, random for the ball + angle of the ball first drop, 
-- computer.screen dimensions, brick colors, brick collisions, win/lose status,
module Breakout exposing (game)

import Playground exposing (..)
import Array
import Random
import Set
import Html exposing (object)

game =
  { initialState = initialState
  , updateState = update
  , view = view
  }

------ PHYSICS PARAMETERS ------

ballRadius = 10
ballColor = black

paddleWidth = 200
paddleHeight = 20
paddleColor = black

brickColors = Array.fromList [rgb 247 0 0, rgb 255 178 46, rgb 247 240 40, rgb 42 252 0, rgb 0 248 252, rgb 252 0 239, rgb 255 171 186]
brickWidth = 70
brickHeight = 30

brickColor: Color
brickColor = red 

screenWidth = 1280
smallerScreenWidth = 638
screenHeight = 559

------ MODEL ------

type alias Ball =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , shape : Shape
  , lives : Int 
  , alive : Bool
  }
type alias Paddle = 
  { x : Float
  , y : Float
  , shape : Shape
  }

type alias Brick = 
  { x : Float
  , y : Float
  , color : Color
  , shape : Shape
  , hit : Bool
  }

type alias Model =
  { ball : Ball
  , paddle : Paddle
  , bricks : List Brick
  }

initialState : Model
initialState =
 { ball =
      { x = 0
      , y = 0
      , dx = 10
      , dy = 10
      , shape = circle ballColor ballRadius
      , lives = 3
      , alive = True
      }
  , paddle =
      { x = 0
      , y = -250
      , shape = rectangle paddleColor paddleWidth paddleHeight
      }
  , bricks = (brickIndices |> List.map getBrick)
  }

brickIndices = 
  [ (0,0) , (1,0) , (2,0) , (3,0) , (4,0) , (5,0) , (6,0) 
  , (0,1) , (1,1) , (2,1) , (3,1) , (4,1) , (5,1) , (6,1)
  , (0,2) , (1,2) , (2,2) , (3,2) , (4,2) , (5,2) , (6,2)
  , (0,3) , (1,3) , (2,3) , (3,3) , (4,3) , (5,3) , (6,3)
  , (0,4) , (1,4) , (2,4) , (3,4) , (4,4) , (5,4) , (6,4)
  , (0,5) , (1,5) , (2,5) , (3,5) , (4,5) , (5,5) , (6,5)
  , (0,6) , (1,6) , (2,6) , (3,6) , (4,6) , (5,6) , (6,6) ]

getBrick: (Float, Float) -> Brick
getBrick (xIndex, yIndex) =
  let
    x = -smallerScreenWidth / 2
    y = screenHeight / 1.5
    xPadding = smallerScreenWidth / 50
    yPadding = screenHeight / 75
  in
    { x = x + (xIndex+1)*brickWidth + xIndex*xPadding
    , y = y - (yIndex+3)*brickHeight - yIndex*yPadding
    , color = (Maybe.withDefault red (Array.get (yIndex |> round) brickColors))
    , shape = rectangle (Maybe.withDefault red (Array.get (yIndex |> round) brickColors)) brickWidth brickHeight
    , hit = False
    }

------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall ballColor ballRadius]
    ++ [model.paddle    |> viewPaddle paddleColor paddleWidth paddleHeight]
    ++ (model.bricks    |> List.map (viewBrick brickWidth brickHeight))

viewBall : Color -> Float -> Ball -> Shape
viewBall color radius obj =
  circle color radius
    |> move obj.x obj.y

viewPaddle : Color -> Float -> Float -> Paddle -> Shape
viewPaddle color width height obj =
  rectangle color width height
    |> move obj.x obj.y


viewBrick : Float -> Float -> Brick -> Shape
viewBrick width height obj =
  rectangle obj.color width height
    |> move obj.x obj.y

------ UPDATE ------

update computer model =
  model
    |> handleMotion computer
    |> checkBrickCollisions  
    |> handleGameLogic computer

handleMotion computer model =
  { model
   | ball = (moveBall computer model model.ball)
   , paddle = (movePaddle computer) model.paddle
  }


handleGameLogic computer model =
  { model
   | ball = (checkWinLose computer model.ball)
  }

moveBall : Computer -> Model -> Ball -> Ball
moveBall computer model ball =
  { ball
    | x = ball.x + ball.dx
    , y = ball.y + ball.dy
    , dx = horizontalBounce computer model
    , dy = verticalBounce computer model
    -- , lives = lifeLogic computer model.ball 
    }


checkWinLose : Computer -> Ball -> Ball
checkWinLose computer ball = 
 { ball
    | lives = lifeLogic computer ball
    ,  alive = ballAlive computer ball
    }

lifeLogic computer ball  =
  let
      lifeReduction = ball.y + ballRadius >= computer.screen.bottom -- check if ball's position is at the bottom of the screen
  in
    if (lifeReduction) then 
      ball.lives - 1
  
    else
      ball.lives


ballAlive : Computer -> Ball -> Bool
ballAlive computer ball  =
  let
      lifeReduction = ball.y + ballRadius >= computer.screen.bottom -- check if ball's position is at the bottom of the screen
  in
    if (lifeReduction) then 
      not ball.alive
  
    else
    ball.alive

brickIsNotHit brick =
  not brick.hit

checkBrickCollisions model =
  { model 
    | bricks = List.filter brickIsNotHit model.bricks}


movePaddle : Computer -> Paddle -> Paddle
movePaddle computer obj =
  { obj | x = computer.mouse.x }

-- this is a helper function written by Julia Kispert to have the ball bounce off the edges of the screen
horizontalBounce: Computer -> Model -> Number
horizontalBounce computer model =
  if ((model.ball.x + model.ball.dx - ballRadius < computer.screen.left) || (model.ball.x + model.ball.dx + ballRadius > computer.screen.right))
  then
    model.ball.dx * (-1)
  else
    model.ball.dx

  
verticalBounce: Computer -> Model -> Number
verticalBounce computer model =
  if ((model.ball.y + model.ball.dy + ballRadius > computer.screen.top))
  then
    model.ball.dy * (-1)
  else
    if (model.ball.y + model.ball.dy + ballRadius < computer.screen.bottom) then 
      0
    else
      bounce model

bounce : Model -> Float
bounce model = 
  let 
    xCollision = (model.ball.x + model.ball.dx > model.paddle.x - paddleWidth/2 && model.ball.x + model.ball.dx < model.paddle.x + paddleWidth/2)
    yCollision = (model.ball.y + model.ball.dy - ballRadius == model.paddle.y + paddleHeight/2)
  in
    if xCollision && yCollision then 
      model.ball.dy * (-1)
    else 
      model.ball.dy
