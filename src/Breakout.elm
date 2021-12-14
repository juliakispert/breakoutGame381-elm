--
-- The classic game of Breakout.
--
-- Authors: Julia Kispert, Betsy Foy, Ayca Arbay

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
initialBallSpeed = 8

paddleWidth = 200
paddleHeight = 20
paddleColor = black

brickColors = Array.fromList [rgb 247 0 0, rgb 255 178 46, rgb 247 240 40, rgb 42 252 0, rgb 0 248 252, rgb 252 0 239, rgb 255 171 186]
brickWidth = 70
brickHeight = 30

screenWidth = 600
screenHeight = 500

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
      , y = -30
      , dx = initialBallSpeed
      , dy = initialBallSpeed
      , shape = circle ballColor ballRadius
      }
  , paddle =
      { x = 0
      , y = -250
      , shape = rectangle paddleColor paddleWidth paddleHeight
      }
  , bricks = (brickCoordinates 6 6 |> List.map getBrick)
  }

brickCoordinates: Int -> Int -> List (Float, Float)
brickCoordinates xs ys =
  List.range 0 ys
    |> List.map toFloat
      |> List.concatMap (\y ->
         List.range 0 xs
          |> List.map toFloat
            |> List.map (\x -> (x, y)))

getBrick: (Float, Float) -> Brick
getBrick (xIndex, yIndex) =
  let
    x = -screenWidth / 2
    y = screenHeight / 1.5
    xPadding = screenWidth / 50
    yPadding = screenHeight / 75
  in
    { x = x + (xIndex + 1) * brickWidth + xIndex * xPadding
    , y = y - (yIndex + 3) * brickHeight - yIndex * yPadding
    , color = (Maybe.withDefault red (Array.get (yIndex |> round) brickColors))
    , shape = rectangle (Maybe.withDefault red (Array.get (yIndex |> round) brickColors)) brickWidth brickHeight
    }

------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall]
    ++ [model.paddle    |> viewPaddle]
    ++ (model.bricks    |> List.map viewBrick)

viewBall : Ball -> Shape
viewBall ball =
  circle ballColor ballRadius
    |> move ball.x ball.y

viewPaddle : Paddle -> Shape
viewPaddle paddle =
  rectangle paddleColor paddleWidth paddleHeight
    |> move paddle.x paddle.y

viewBrick : Brick -> Shape
viewBrick brick =
  rectangle brick.color brickWidth brickHeight
    |> move brick.x brick.y

------ UPDATE ------

update computer model =
  model
    |> handleMotion computer
    |> updateModel

handleMotion computer model =
  { model
   | ball = (moveBall computer model model.ball)
   , paddle = (movePaddle computer model.paddle)
  }

moveBall : Computer -> Model -> Ball -> Ball
moveBall computer model ball =
  { ball
    | x = ball.x + ball.dx
    , y = ball.y + ball.dy
    , dx = horizontalBounce computer model
    , dy = verticalBounce computer model
    }

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
      bounceOffPaddle model

bounceOffPaddle : Model -> Float
bounceOffPaddle model = 
  let 
    xCollision = (model.ball.x + model.ball.dx > model.paddle.x - paddleWidth/2 && model.ball.x + model.ball.dx < model.paddle.x + paddleWidth/2)
    yCollision = (model.ball.y + model.ball.dy - ballRadius == model.paddle.y + paddleHeight/2)
  in
    if xCollision && yCollision then 
      model.ball.dy * (-1)
    else 
      model.ball.dy

movePaddle : Computer -> Paddle -> Paddle
movePaddle computer paddle =
  { paddle | x = computer.mouse.x }
  
updateModel model =
  let
    t = checkBrickCollisions model
  in
    { model 
      | ball = Tuple.first t
      , bricks = Tuple.second t
    }

checkBrickCollisions model =
  List.foldl checkBrickCollision (model.ball, []) model.bricks

checkBrickCollision: Brick -> (Ball, List Brick) -> (Ball, List Brick)
checkBrickCollision brick (ball, bricks) =
  let
    ballBottom = ball.y + ball.dy - ballRadius
    ballTop = ball.y + ball.dy + ballRadius
    ballLeft = ball.x + ball.dx - ballRadius
    ballRight = ball.x + ball.dx + ballRadius

    brickBottom = brick.y - brickHeight/2
    brickTop = brick.y + brickHeight/2
    brickLeft = brick.x - brickWidth/2
    brickRight = brick.x + brickWidth/2
  
    verticalCollision = (ballRight >= brickLeft && ballLeft <= brickRight) && ((ballTop >= brickBottom && ballTop <= brickTop) || (ballBottom <= brickTop && ballBottom >= brickBottom))
    horizontalCollision = (ballTop >= brickBottom && ballBottom <= brickTop) && ((ballRight >= brickLeft && ballRight <= brickRight) || (ballLeft <= brickRight && ballLeft >= brickLeft))
  in
    if verticalCollision 
    then
      ({ ball | dy = -ball.dy }, bricks )
    else if horizontalCollision
    then
      ({ ball | dx = -ball.dx }, bricks )
    else
      (ball, brick :: bricks)
