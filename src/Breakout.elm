--
-- The classic game of Breakout.
--
-- Authors: Julia Kispert, Betsy Foy, Ayca Arbay

module Breakout exposing (game)

import Playground exposing (..)
import Array
import Set

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
    }

------ VIEW ------
view: Computer -> Model -> List Shape
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
viewHelpMessage : Computer -> Shape -> Shape
viewHelpMessage computer helpMessage = 
  helpMessage
    |> move 0 (computer.screen.top - paddleHeight)

------ UPDATE ------
update : Computer -> Model -> Model 
update computer model = 
  case model.state of 
    Playing -> playingUpdate computer model 
    GameOver -> gameOverUpdate computer model 
    NewRound -> newRoundUpdate computer model

playingUpdate : Computer -> Model -> Model 
playingUpdate computer model =
  model
    |> handleMotion computer
    |> checkDeath computer
    |> updateBallAndBricks


newRoundUpdate : Computer -> Model -> Model 
newRoundUpdate computer model= 
  model 
    |> handlePause computer

gameOverUpdate : Computer -> Model -> Model 
gameOverUpdate computer model = 
  model 
    |> endGame computer 

-- Handles end of Game state and allows player to play again if 'P' key is pressed 
endGame : Computer -> Model -> Model 
endGame computer model = 
   if keyPressed "P" computer then 
    initialState
  else 
    model

-- When waiting for new round (when game is paused between rounds), handles switch to state of playing when R key is pressed
handlePause : Computer -> Model -> Model 
handlePause computer model= 
  if keyPressed "R" computer then 
    { model 
        | state = Playing 
        , helpMessage = words white " " 
      }
  else 
    model

-- Handles all continous motion of Ball (besides when it hits a Brick) and Paddle
handleMotion : Computer -> Model -> Model          
handleMotion computer model =
  { model
   | ball = (moveBall computer model model.ball)
   , paddle = (movePaddle computer model.paddle)
   }

-- Handles all the movement ball makes (besides when it needs to bounce off a brick which is handled seperately to allow us to remove Bricks and change velocity together)
moveBall : Computer -> Model -> Ball -> Ball
moveBall computer model ball =
  { ball
    | x = ball.x + ball.dx
    , y = ball.y + ball.dy
    , dx = horizontalBounce computer model
    , dy = verticalBounce computer model
    }

-- Handles if Ball bounces off sizes of the screen 
horizontalBounce: Computer -> Model -> Number
horizontalBounce computer model =
  if ((model.ball.x + model.ball.dx - ballRadius < computer.screen.left) || (model.ball.x + model.ball.dx + ballRadius > computer.screen.right))
  then
    model.ball.dx * (-1)
  else
    model.ball.dx

-- Handles how ball bounces vertically (either off top of screen or off Paddle)
verticalBounce: Computer -> Model -> Number
verticalBounce computer model =
  if ((model.ball.y + model.ball.dy + ballRadius > computer.screen.top))
  then
    model.ball.dy * (-1)
  else
    bounceOffPaddle model

-- Checks if Ball needs to bounce of Paddle
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

-- Moves Paddle based on computer mouse x position 
movePaddle : Computer -> Paddle -> Paddle
movePaddle computer paddle =
  { paddle | x = computer.mouse.x }

-- Checks if Game needs to be paused for NewRound (lost of a life), if the Game is over as no lives left, or if the Game has been one (no more Bricks)
checkDeath : Computer -> Model -> Model
checkDeath computer model = 
  if (model.ball.y + ballRadius < computer.screen.bottom)
  then 
    { model
      | ball = initialBall
      , state = NewRound
      , lives = model.lives - 1
      , helpMessage = words black "Press R to start next round"
      }
  else if (model.lives == 0)
  then 
    { model 
      | state = GameOver
      , helpMessage = words black "Game Over, You Lost. Press P to play again."
      }
  else if (List.length model.bricks == 0) 
  then 
    { model 
      | state = GameOver
      , helpMessage = words black "Game Over, You Won. Press P to play again."
      }
  else
    model

-- Updates Model by forming tuple of Ball and Bricks to check for collision 
updateBallAndBricks : Model -> { ball : Ball, paddle : Paddle, bricks : List Brick, state : GameState, helpMessage : Shape, lives : Int }
updateBallAndBricks model =
  let
    t = handleBrickCollisions model
  in
    { model 
      | ball = Tuple.first t
      , bricks = Tuple.second t
    }

-- Handles updating Ball and each Brick within Bricks 
handleBrickCollisions : Model -> (Ball, List Brick)
handleBrickCollisions model =
  List.foldl checkBrickCollision (model.ball, []) model.bricks


-- Checks if any part of Ball lies within a Brick, meaning it has been hit before causing the Ball to bounce and Brick to be removed/hit   
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

-- Helper Methods from Asteroids Programming Languages Homework -- 
keyPressed : String -> Computer -> Bool
keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)
    