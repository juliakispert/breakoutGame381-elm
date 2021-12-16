# breakoutGame381-elm
### Overview. 

Breakout was developed in 1976 by Steve Wozniak, shortly before he cofounded Apple Computer, Inc. He cited the experience of building the game as part of his inspiration in creating the Apple ][, the first commercially viable home computer.

The game
The initial configuration of the Breakout game appears in the image below. The colored rectangles aka the bricks (red, orange, yellow, green, cyan, magenta, pink) are the targets.

The slightly larger rectangle at the bottom is the paddle. The paddle is in a fixed position in the vertical dimension, but moves back and forth horizontally across the screen along with the mouse until it reaches the edge of its space.

The ball will be released at an random angle and your job is to move the paddle with your mouse to prevent the ball from going off the screen and hit all the bricks. When the ball hits a brick it will disapear The ball will have 3 lives. When the ball goes of the screen this signifies the loss of life, and a new round to start. To start the next round press "R" key on your keyboard. If the game ends at any point press "P" key to play again.

Note: the trajectory of the ball will change based on where it hits the brick, the paddle or the sides of the screen. 

## You will lose if: 
- you run out of lives (ball goes off the screen 3 times) 

## You will win if: 
- you hit all the bricks/ no more bricks on the screen

<img width="1114" alt="Screen Shot 2021-12-15 at 3 58 40 PM" src="https://user-images.githubusercontent.com/54856485/146271636-5f4d693e-107a-43bb-ab78-33a97ec98ca7.png">


## How to Play 
To launch project run this in terminal: 
elm-live --open -- src/Main.elm --output=elm.js


In depth Overview and Explanation: 

Our Elm implementation of Breakout Game relies on files, Breakout and Main, though most of the work is done in Breakout. In Breakout we divided our code into 5 sections: Feature Parameters, Model, View, Update, and Helper Methods. These sections are piped to Playground Game Application from the Playground Elm library, to make our code runnable in Elm.
In the Feature Parameter section we define variables that are constant throughout the project and primary features of the objects used in the model, including ball color, paddle height, and an array of colors used for the brick wall. 
The Types section builds one type—GameState—and 4 type aliases: Ball, Paddle, Brick, and Model. The Ball is represented on screen by a Playground shape object—a circle, in this case—and the type alias contains its x and y coordinates, and its velocity (dx and dy). The Paddle is represented by another Playground shape object—a rectangle—and its alias only holds its x and y coordinates. Like Paddle, Brick’s type alias is also displayed as a rectangle Playground shape object and its type alias holds x and y coordinates, but it also includes color. The most important type alias is the Model which holds all the features of the game: the ball (type alias Ball), paddle (type alias Paddle), bricks (list of type alias Brick), state (type GameState), helpMessage (type Playground shape), and lives (float). The helpMessage displays text to let the user know if they won/lost a round and lives represents how many lives the user currently has. The GameState type represents if the game is currently being played, waiting for the next round to start, or is over. The Type section also sets up the wall of all the bricks, mapping each brick to their proper location, with their proper color, based on their index in the list of bricks from the model. 
The View section puts every element from the model on the screen. To put the list of bricks on the screen, we use the viewBrick method that displays a single brick shape and map it on the game’s current list of bricks.
The Update section handles the bulk of the logic to run the game. The update call handles the game based on the state of the model. If the model state is “Playing”, the game is updated with three major key components. These components are handlingMotion, checkDeath and updateBallAndBricks. HandlingMotion controls most of the ball and paddle’s movements, allowing the paddle to be controlled by the mouse and the ball to bounce off both the sides of the screen and the paddle. This method does not handle how the ball bounces off a brick. CheckDeath constantly checks if the user has won or lost the game, or if a round is over. If any of these is true, CheckDeath properly updates the model to represent the new state the user has entered. UpdateBallAndBricks handles ball-brick collisions, using helper methods that remove bricks and change the velocity of the ball based on where it hit the brick. The next model state is GameOver. This state is managed by the endGame functions which allows the user to press “P” on their keyboard to play again. If the user presses “P”, the model is updated back to its initial state. If not, the game persists in the GameOver state. The last state is NewRound. This state is represented by the function handlePause which allows the user to press “R” on their keyboard to initiate the next round, and the GameState is returned to Playing. 
The last section of the game is Helper Methods which is only keyPressed. This method allows the player to press the “P” and “R” key to play again or start a new round. 

