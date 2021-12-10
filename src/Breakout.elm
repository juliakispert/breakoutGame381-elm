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

bulletRadius = 2

ballRadius = 10

ballColor = black

intialBallSpeed = 0.75


------ MODEL ------

type alias GameObject = 
 { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , radius : Float
  , shape : Shape
  }
type alias Model =
  { ball : GameObject
  }
initialState : Model
initialState =
  { ball = 
      { x = 0
      , y = 0
      , dx = 10
      , dy = 10
      , radius = 10
      , shape = circle ballColor ballRadius
  }
  }


------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall ballColor ballRadius]

viewBall : Color -> Float -> GameObject -> Shape
viewBall color radius obj =
  circle color radius
    |> move obj.x obj.y

------ UPDATE ------

update computer model =
  model
    |> handleMotion computer


handleMotion computer model =
  { model
    | ball =  (moveObject computer) model.ball
  }

  
moveObject : Computer -> GameObject -> GameObject
moveObject computer obj =
  { obj
    | x = obj.x + obj.dx
    , y = obj.y + obj.dy
    , dx = bounce (obj.x + obj.dx - ballRadius < computer.screen.left) (obj.x + obj.dx + ballRadius > computer.screen.right) obj.dx
    , dy = bounce (obj.y + obj.dy - ballRadius< computer.screen.bottom) (obj.y + obj.dy + ballRadius > computer.screen.top ) obj.dy
  }

-- this is a helper function written by Julia Kispert to have the ball bounce off the edges of the screen 
bounce min max dx = 
  if (min || max) 
  then 
    dx * (-1) 
  else 
    dx