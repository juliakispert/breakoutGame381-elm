--
-- The classic game of Asteroids, minus the pesky problem of dying.
--
-- Author: Paul Cantrell
--
module Breakout exposing (game)

import Playground exposing (..)
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

paddleWidth = 80
paddleHeight = 20
paddleColor = black

brickColor = red
brickWidth = 70
brickHeight = 15

screenWidth = 1280
smallerScreenWidth = 638
screenHeight = 559

------ MODEL ------

type alias GameObject =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , shape : Shape
  }

type alias Model =
  { ball : GameObject
  , paddle : GameObject
  , bricks : List GameObject
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
      , dx = 0
      , dy = 0
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

getBrick: (Float, Float) -> GameObject
getBrick (xIndex, yIndex) =
  let
    x = -smallerScreenWidth / 2
    y = screenHeight / 2
    xPadding = smallerScreenWidth / 50
    yPadding = screenHeight / 100
  in
    { x = x + (xIndex+1)*brickWidth + xIndex*xPadding
    , y = y - (yIndex+3)*brickHeight - yIndex*yPadding
    , dx = 0
    , dy = 0
    , shape = rectangle brickColor brickWidth brickHeight
    }

------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall ballColor ballRadius]
    ++ [model.paddle    |> viewRectangle paddleColor paddleWidth paddleHeight]
    ++ (model.bricks    |> List.map (viewRectangle brickColor brickWidth brickHeight))

viewBall : Color -> Float -> GameObject -> Shape
viewBall color radius obj =
  circle color radius
    |> move obj.x obj.y

viewRectangle : Color -> Float -> Float -> GameObject -> Shape
viewRectangle color width height obj =
  rectangle color width height
    |> move obj.x obj.y

------ UPDATE ------

update computer model =
  model
    |> handleMotion computer
    

handleMotion computer model =
  { model
   | ball = (moveObject computer) model.ball
   , paddle = (movePaddle computer) model.paddle
  }

moveObject : Computer -> GameObject -> GameObject
moveObject computer obj =
  { obj
    | x = obj.x + obj.dx
    , y = obj.y + obj.dy
    , dx = bounce (obj.x + obj.dx - ballRadius < computer.screen.left) (obj.x + obj.dx + ballRadius > computer.screen.right) obj.dx
    , dy = bounce (obj.y + obj.dy - ballRadius < computer.screen.bottom) (obj.y + obj.dy + ballRadius > computer.screen.top ) obj.dy
    }

movePaddle : Computer -> GameObject -> GameObject
movePaddle computer obj =
  { obj | x = computer.mouse.x }

-- this is a helper function written by Julia Kispert to have the ball bounce off the edges of the screen

bounce min max dx =
  if (min || max)
  then
    dx * (-1)
  else
    dx