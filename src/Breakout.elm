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
  , color : Color
  , shape : Shape
  , hit : Bool
  }

type GameState
  = Playing
  | GameOver
  | NewRound

type alias Model =
  { ball : Ball
  , paddle : Paddle
  , bricks : List Brick
  , state : GameState
  , helpMessage : Shape
  , lives : Int
  }

initialState : Model
initialState =
  { ball = initialBall
  , paddle =
    { x = 0
    , y = -250
    , shape = rectangle paddleColor paddleWidth paddleHeight
    }
  , bricks = (brickCoordinates 6 6 |> List.map getBrick)
  , state = Playing
  , helpMessage = words white " "
  , lives = 3
  }

initialBall = 
  { x = 0
  , y = 0
  , dx = 10
  , dy = -10
  , shape = circle ballColor ballRadius
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
    y = screenHeight / 1.25
    xPadding = screenWidth / 50
    yPadding = screenHeight / 75
  in
    { x = x + (xIndex + 1) * brickWidth + xIndex * xPadding
    , y = y - (yIndex + 3) * brickHeight - yIndex * yPadding
    , color = (Maybe.withDefault red (Array.get (yIndex |> round) brickColors))
    , shape = rectangle (Maybe.withDefault red (Array.get (yIndex |> round) brickColors)) brickWidth brickHeight
    , hit = False
    }

------ VIEW ------

view computer model =
  [rectangle white computer.screen.width computer.screen.height]
    ++ [model.ball      |> viewBall]
    ++ [model.paddle    |> viewPaddle]
    ++ (model.bricks    |> List.map viewBrick)
    ++ [model.helpMessage |> viewHelpMessage computer]

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


viewHelpMessage computer helpMessage = 
  helpMessage
    |> move 0 (computer.screen.top - paddleHeight)
  

------ UPDATE ------

update computer model = 
  case model.state of 
    Playing -> playingUpdate computer model 
    GameOver -> gameOverUpdate computer model 
    NewRound -> newRoundUpdate computer model
playingUpdate computer model =
  model
    |> handleMotion computer
    |> checkDeath computer
    |> checkBrickCollisions
    |> removeBricks

newRoundUpdate computer model= 
  model 
    |> handlePause computer


gameOverUpdate computer model = 
  model 
    |> endGame computer 

endGame computer model = 
   if keyPressed "P" computer then 
    initialState
  else 
    model
  

handlePause computer model= 
  if keyPressed "R" computer then 
    { model 
        | state = Playing 
        , helpMessage = words white " " 
      }
  else 
    model
          
checkDeath computer model = 
  if (model.ball.y + ballRadius < computer.screen.bottom) then 
    { model
      | ball = initialBall
      , state = NewRound
      , lives = model.lives - 1
      , helpMessage = words black "Press R to start next round"
      }
  else
    if (model.lives == 0) then 
      { model 
        | state = GameOver
        , helpMessage = words black "Game Over, You Lost. Press P to play again."
        }
    else 
      if (List.length model.bricks == 0) then 
        { model 
        | state = GameOver
        , helpMessage = words black "Game Over, You Won. Press P to play again."
        }
      else
        model

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

checkBrickCollisions model =
  { model
    | bricks = List.filter brickIsNotHit model.bricks}

brickIsNotHit brick =
  not brick.hit

removeBricks model =
  { model
    | bricks = (model.bricks |> List.map (removeBrick model.ball)) }

removeBrick ball brick =
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
  in
    if (xRange && yRange)
    then
      brickGetsHit brick
    else
      brick

brickGetsHit: Brick -> Brick
brickGetsHit brick =
  { brick
    | hit = True }

-- Helper Methods from Asteroids Programming Languages Homework -- 
keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)

-- bounceOffBricks bricks ball =
--   bricks |> List.map (bounceOffBrick ball)

-- bounceOffBrick ball brick =
--   let
--     ballBottom = ball.y + ball.dy - ballRadius
--     ballTop = ball.y + ball.dy + ballRadius
--     ballLeft= ball.x + ball.dx - ballRadius
--     ballRight = ball.x + ball.dx + ballRadius

--     brickBottom = brick.y - brickHeight/2
--     brickTop = brick.y + brickHeight/2
--     brickLeft = brick.x - brickWidth/2
--     brickRight = brick.x + brickWidth/2

--     xRange = (ballRight >= brickLeft && ballLeft <= brickRight)
--     yRange = (ballTop >= brickBottom && ballBottom <= brickTop)
  
--     bottomCollision = (xRange && (ballTop >= brickBottom))
--     topCollision = (xRange && (ballBottom <= brickTop))
--     leftCollision = (yRange && (ballRight >= brickLeft))
--     rightCollision = (yRange && (ballLeft <= brickRight))
--   in
--     if (topCollision || bottomCollision)
--     then
--       0
--     else if (rightCollision || leftCollision)
--     then
--       1
--     else
--       2

-- bounceOffScreen computer ball =
--   let
--     nextX = ball.x + ball.dx
--     nextY = ball.y + ball.dy

--     minX = computer.screen.left + ballRadius
--     maxX = computer.screen.right - ballRadius
--     maxY = computer.screen.top - ballRadius

--     xInBounds = (nextX >= minX && nextX <= maxX)
--     yInBounds = nextY <= maxY
--   in
--     if (xInBounds && yInBounds) then
--       { ball
--         | x = nextX
--         , y = nextY }
--     else if (xInBounds) then
--       { ball
--         | x = nextX
--         , y = nextY 
--         , dy = -ball.dy }
--     else
--       { ball
--         | x = nextX
--         , y = nextY 
--         , dx = -ball.dx }

-- bounceOffPaddle paddle ball =
--   let
--     nextX = ball.x + ball.dx
--     nextY = ball.y + ball.dy

--     minX = paddle.x - paddleWidth/2
--     maxX = paddle.x + paddleWidth/2
--     maxY = paddle.y + paddleHeight/2 + ballRadius

--     xInBounds = (nextX >= minX && nextX <= maxX)
--     yInBounds = nextY <= maxY
--   in
--     if (xInBounds && yInBounds) then
--       { ball
--           | x = nextX
--           , y = nextY 
--           , dy = -ball.dy }
--     else
--       ball
