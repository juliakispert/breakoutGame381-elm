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
