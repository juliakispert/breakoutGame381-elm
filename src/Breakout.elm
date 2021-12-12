--
-- The classic game of Asteroids, minus the pesky problem of dying.
--
-- Author: Paul Cantrell
-- documentation, comments, random for the ball + angle of the ball first drop, 
-- computer.screen dimensions, brick colors, brick collisions, win/lose status,
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
  , shape : Shape
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

getBrick: (Float, Float) -> Brick
getBrick (xIndex, yIndex) =
  let
    x = -smallerScreenWidth / 2
    y = screenHeight / 2
    xPadding = smallerScreenWidth / 50
    yPadding = screenHeight / 100
  in
    { x = x + (xIndex+1)*brickWidth + xIndex*xPadding
    , y = y - (yIndex+3)*brickHeight - yIndex*yPadding
    , shape = rectangle brickColor brickWidth brickHeight
    }

------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall ballColor ballRadius]
    ++ [model.paddle    |> viewPaddle paddleColor paddleWidth paddleHeight]
    ++ (model.bricks    |> List.map (viewBrick brickColor brickWidth brickHeight))

viewBall : Color -> Float -> Ball -> Shape
viewBall color radius obj =
  circle color radius
    |> move obj.x obj.y

viewPaddle : Color -> Float -> Float -> Paddle -> Shape
viewPaddle color width height obj =
  rectangle color width height
    |> move obj.x obj.y


viewBrick : Color -> Float -> Float -> Brick -> Shape
viewBrick color width height obj =
  rectangle color width height
    |> move obj.x obj.y

------ UPDATE ------

update computer model =
  model
    |> handleMotion computer
    -- |> findCollided model.ball model.paddle
    

handleMotion computer model =
  { model
   | ball = (moveObject computer) model.ball
   , paddle = (movePaddle computer) model.paddle
  }
findCollided ball paddle =
 (hypot (ball.x - paddle.x) (ball.y - paddle.y)) <= ball.radius + paddle.radius

objectsCollide obj0 obj1 =
  (hypot (obj0.x - obj1.x) (obj0.y - obj1.y)) <= obj0.radius + obj1.radius

anyCollide otherShapes shape =
  List.any (objectsCollide shape) otherShapes

moveObject : Computer -> Ball -> Ball
moveObject computer obj =
  { obj
    | x = obj.x + obj.dx
    , y = obj.y + obj.dy
    , dx = bounce (obj.x + obj.dx - ballRadius < computer.screen.left) (obj.x + obj.dx + ballRadius > computer.screen.right) obj.dx
    , dy = bounce (obj.y + obj.dy - ballRadius < computer.screen.bottom) (obj.y + obj.dy + ballRadius > computer.screen.top ) obj.dy
    }

movePaddle : Computer -> Paddle -> Paddle
movePaddle computer obj =
  { obj | x = computer.mouse.x }

-- this is a helper function written by Julia Kispert to have the ball bounce off the edges of the screen

bounce min max dx =
  if (min || max)
  then
    dx * (-1)
  else
    dx
-- Created by Paul Cantrel via Asteroid Assignment
hypot x y =  -- mysteriously, Elm doesn't have this built in
  toPolar (x, y) |> Tuple.first