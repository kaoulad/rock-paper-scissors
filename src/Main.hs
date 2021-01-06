module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- Title game, Boolean for know if is game started,CPU picture, Score, User picture
type Model = (String, Bool, Picture, Int, Picture)

-------------------------------------

window :: Display
window = InWindow "Rock Paper Scissors" (800,600) (200,200)

background :: Color
background = white

fps :: Int
fps = 60

------------------------------------

randomChoiceCPU :: IO Int -> Picture -> Picture -> Picture -> IO (Picture, Int)
randomChoiceCPU v rock paper scissors = do
                                          n <- v
                                          if n == 1 then pure (rock,1) else if n == 2 then pure (paper,2) else pure (scissors,3)

whoWon :: String -> Int -> String
whoWon "rock" 1 = "Equality !"
whoWon "rock" 2 = "You lost..."
whoWon "rock" 3 = "You won !"
whoWon "paper" 1 = "You won !"
whoWon "paper" 2 = "Equality !"
whoWon "paper" 3 = "You lost..."
whoWon "scissors" 1 = "You lost..."
whoWon "scissors" 2 = "You won !"
whoWon "scissors" 3 = "Equality !"

whoWonScore :: String -> Int -> Int -> Int
whoWonScore "rock" 1 k = k
whoWonScore "rock" 2 k = if k == 0 then k else k-1
whoWonScore "rock" 3 k = k+1
whoWonScore "paper" 1 k = k+1
whoWonScore "paper" 2 k = k
whoWonScore "paper" 3 k = if k == 0 then k else k-1
whoWonScore "scissors" 1 k = if k == 0 then k else k-1
whoWonScore "scissors" 2 k = k+1
whoWonScore "scissors" 3 k = k

-------------------------------------

draw :: Picture -> Picture -> Picture -> Model -> IO Picture
draw rock paper scissors (s, b, w, k,u) = if b then pure (Pictures [textPic s, setInfos "'r' key for rock | 'p' key for paper | 's' key for scissors", setTextUser, setTextCPU, picUser u, picCPU w, score "Score:"]) else pure (Pictures [textPic s,setInfos "'r' key for rock | 'p' key for paper | 's' key for scissors", setTextUser, setTextCPU])
        where textPic t = Scale 0.5 0.5 (Translate (-700) 410 (Text t))
              setInfos t = Color red $ Scale 0.15 0.15 (Translate (-1980) 1000 (Text t))
              setTextUser = Scale 0.15 0.15 (Translate (-1200) (-1300) (Text "User"))
              setTextCPU = Scale 0.15 0.15 (Translate (-1200) 100 (Text "CPU"))
              -------
              picUser n = Translate (0) (-200) n
              picCPU l = Translate 0 10 l
              score m = Color blue  $ Scale 0.2 0.2 (Translate (50) 1050 (Text (m++show k)))

-------------------------------------
  

inputHandler :: Picture -> Picture -> Picture -> Event -> Model -> IO Model  
inputHandler rock paper scissors (EventKey (Char 'r') Down _ _) (t,b,w,k,u) = do                               
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "rock" (snd $ tuple_) k
                                                                            return (whoWon "rock" (snd $ tuple_), True, (fst $ tuple_), score,rock)

inputHandler rock paper scissors (EventKey (Char 'p') Down _ _) (t,b,w,k,u) = do
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "paper" (snd $ tuple_) k
                                                                            return (whoWon "paper" (snd $ tuple_), True, (fst $ tuple_), score,paper)

inputHandler rock paper scissors (EventKey (Char 's') Down _ _ ) (t,b,w,k,u) = do
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "scissors" (snd $ tuple_) k
                                                                            return (whoWon "scissors" (snd $ tuple_), True, (fst $ tuple_), score,scissors)
inputHandler _ _ _ _ (t,b,w,k,u) = return (t,b,w,k,u)


updateFunc :: Float -> Model -> IO Model
updateFunc _ (t,b,w,k,u) = pure (t,b,w,k,u)

-------------------------------------

main :: IO ()
main = do
        rock <- loadBMP "src/assets/rock.bmp"
        paper <- loadBMP "src/assets/paper.bmp"
        scissors <- loadBMP "src/assets/scissors.bmp"

        playIO window background fps ("Rock Paper Scissors", False, Blank, 0, Blank) (draw rock paper scissors) (inputHandler rock paper scissors) updateFunc