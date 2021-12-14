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

game =
  { initialState = initialState
  , updateState = update
  , view = view
  }

------ PHYSICS PARAMETERS ------

ballRadius = 10
ballColor = black
initialBallSpeed = 0.75

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
  }

type alias Paddle =
  { x : Float
  , y : Float
  , shape : Shape
  }

type alias Brick =
  { x : Float
  , y : Float
  , color: Color
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

{-| playingAround : Int -> Int -> List (Int, Int)
playingAround xs ys =
  List.range 0 ys
    |> List.concatMap (\y ->
      List.range 0 xs
        |> List.map (\x -> (x, y)))
-}

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
    --++ [words black (Debug.toString(playingAround 6 6)) |> move 0 0]

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

handleMotion computer model =
  { model
   | ball = (moveBall computer model model.ball)
   , paddle = (movePaddle computer) model.paddle
  }

moveBall : Computer -> Model -> Ball -> Ball
moveBall computer model ball =
  { ball
    | x = ball.x + ball.dx
    , y = ball.y + ball.dy
    , dx = ball.dx
    , dy = ball.dy
    }

brickIsNotHit brick =
  not brick.hit

checkBrickCollisions model =
  { model
    | bricks = List.filter brickIsNotHit model.bricks}

movePaddle : Computer -> Paddle -> Paddle
movePaddle computer obj =
  { obj | x = computer.mouse.x }

-- this is a helper function written by Julia Kispert to have the ball bounce off the edges of the screen
bounceOffScreen computer ball =
  let
    nextX = ball.x + ball.dx
    nextY = ball.y + ball.dy

    leftBound = computer.screen.left + ballRadius
    rightBound = computer.screen.right - ballRadius
    topBound = computer.screen.top - ballRadius
  in
    if ((nextX < leftBound) || (nextX > rightBound))
    then
      switchDx ball
    else if (nextY > topBound)
    then
      switchDy ball
    else
      ball

bounceOffPaddle paddle ball =
  let
    nextX = ball.x + ball.dx
    nextY = ball.y + ball.dy

    paddleLeft = paddle.x - paddleWidth/2
    paddleRight = paddle.x + paddleWidth/2
    paddleTop = paddle.y + paddleHeight/2 + ballRadius
  in
    if ((nextX >= paddleLeft && nextX <= paddleRight) && (nextY <= paddleTop))
    then
      switchDy ball
    else
      ball

bounceOffBrick computer ball brick =
  let
    ballBottom = ball.y + ball.dy - ballRadius
    ballTop = ball.y + ball.dy + ballRadius
    ballLeft= ball.x + ball.dx - ballRadius
    ballRight = ball.x + ball.dx + ballRadius

    brickBottom = brick.y - brickHeight/2
    brickTop = brick.y + brickHeight/2
    brickLeft = brick.x - brickWidth/2
    brickRight = brick.x + brickWidth/2

    xRange = (ballRight >= brickLeft && ballLeft <= brickRight)
    yRange = (ballTop >= brickBottom && ballBottom <= brickTop)

    bottomCollision = (xRange && (ballTop >= brickBottom))
    topCollision = (xRange && (ballBottom <= brickTop))
    leftCollision = (yRange && (ballRight >= brickLeft))
    rightCollision = (yRange && (ballLeft <= brickRight))
  in
    if (topCollision || bottomCollision)
    then
      brickIsHit brick
      switchDy ball
    else if (rightCollision || leftCollision)
    then
      brickIsHit brick
      switchDx ball
    else
      ball

switchDx ball =
  { ball
    | dx = ball.dx * (-1) }

switchDy ball =
  { ball
    | dy = ball.dy * (-1) }

brickIsHit brick =
  { brick
    | hit = True }

--bounce : Float -> Bool -> Bool -> Float
-- Created by Paul Cantrel via Asteroid Assignment
-- have to change it so ball falls off screen