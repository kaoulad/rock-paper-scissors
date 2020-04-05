module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

type Model = (String, Bool, Picture, Int)

-------------------------------------

window :: Display
window = InWindow "Rock Paper Scissors" (800,600) (200,200)

background :: Color
background = white

fps :: Int
fps = 20

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
whoWonScore "rock" 2 k = k-1
whoWonScore "rock" 3 k = k+1
whoWonScore "paper" 1 k = k+1
whoWonScore "paper" 2 k = k
whoWonScore "paper" 3 k = k-1
whoWonScore "scissors" 1 k = k-1
whoWonScore "scissors" 2 k = k+1
whoWonScore "scissors" 3 k = k

-------------------------------------

draw :: Picture -> Picture -> Picture -> Model -> IO Picture
draw rock paper scissors (s, b, w, k) = if b then pure (Pictures [textPic s, rockPic rock, paperPic paper, scissorsPic scissors, picCPU w, label1, label2, label3, score "Score:"]) else pure (Pictures [textPic s, rockPic rock, paperPic paper, scissorsPic scissors, label1, label2, label3])
        where textPic t = Scale 0.5 0.5 (Translate (-700) 410 (Text t))
              rockPic r = Translate (-200) (-150) r
              paperPic p = Translate (0) (-150) p
              scissorsPic o = Translate (200) (-150) o
              picCPU l = Translate 0 80 l
              label1 = Scale 0.5 0.5 (Translate (-450) (-560) (Text "r"))
              label2 = Scale 0.5 0.5 (Translate (-25) (-560) (Text "p"))
              label3 = Scale 0.5 0.5 (Translate (360) (-560) (Text "s"))
              score m = Color blue (Scale 0.2 0.2 (Translate (50) 1050 (Text (m++show k))))

-------------------------------------
  

inputHandler :: Picture -> Picture -> Picture -> Event -> Model -> IO Model  
inputHandler rock paper scissors (EventKey (Char 'r') Down _ _) (t,b,w,k) = do                               
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "rock" (snd $ tuple_) k
                                                                            return (whoWon "rock" (snd $ tuple_), True, (fst $ tuple_), score)

inputHandler rock paper scissors (EventKey (Char 'p') Down _ _) (t,b,w,k) = do
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "paper" (snd $ tuple_) k
                                                                            return (whoWon "paper" (snd $ tuple_), True, (fst $ tuple_), score)

inputHandler rock paper scissors (EventKey (Char 's') Down _ _ ) (t,b,w,k) = do
                                                                            tuple_ <- randomChoiceCPU (randomRIO (1,3)) rock paper scissors
                                                                            let score= whoWonScore "scissors" (snd $ tuple_) k
                                                                            return (whoWon "scissors" (snd $ tuple_), True, (fst $ tuple_), score)
inputHandler _ _ _ _ (t,b,w,k) = return (t,b,w,k)


updateFunc :: Float -> Model -> IO Model
updateFunc _ (t,b,w,k) = pure (t,b,w, k)

-------------------------------------

main :: IO ()
main = do
        rock <- loadBMP "src/assets/rock.bmp"
        paper <- loadBMP "src/assets/paper.bmp"
        scissors <- loadBMP "src/assets/scissors.bmp"

        playIO window background fps ("Rock Paper Scissors", False, Blank, 0) (draw rock paper scissors) (inputHandler rock paper scissors) updateFunc