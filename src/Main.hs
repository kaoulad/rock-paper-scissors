module Main where

---
import Control.Monad.Random
import Control.Monad.Trans.RWS (RWS, runRWS, rws)
import Control.Monad.Trans.State
import Game.Pictures
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-------------------------------------------------------------------

data Move = Rock | Paper | Scissors deriving (Eq, Enum, Show)

data Round = Round {playerMove, aiMove :: Move}

data Assets = Assets {rockPicture, paperPicture, scissorsPicture :: Picture}

data Model = Model
  { running :: Bool,
    rounds :: [Round],
    userScore :: Int,
    aiScore :: Int
  }

type View a = RWS Assets Picture () a

type Controller a = StateT Model (Rand StdGen) a

----------------------------------------------------------------------
--------------------------- Utils ------------------------------------
----------------------------------------------------------------------

--Rock <= Paper ; Paper <= Scissors ; Scissors <= Rock
winner :: Move -> Move -> Ordering
winner user ai
  | user == ai = EQ
  | (user == Rock && ai == Paper) || (user == Paper && ai == Scissors) || (user == Scissors && ai == Rock) = LT
  | otherwise = GT

--------------

randomMove :: Controller Move
randomMove = do
  randomNumber <- getRandomR (0, 2)
  return $ toEnum (fromInteger randomNumber)

putState :: Move -> Controller ()
putState move = do
  (Model running rounds userScore aiScore) <- get
  aiMove <- randomMove
  case length rounds of
    3 -> put $ Model False [] 0 0
    _ -> case winner move aiMove of
      EQ -> put $ Model True ((Round move aiMove) : rounds) userScore aiScore -- Add one more round
      LT -> put $ Model True ((Round move aiMove) : rounds) userScore (aiScore + 1)
      GT -> put $ Model True ((Round move aiMove) : rounds) (userScore + 1) aiScore
  return ()

--------------

pic :: Move -> Assets -> Picture
pic move assets = case move of
  Rock -> rock
  Paper -> paper
  Scissors -> scissors
  where
    (Assets rock paper scissors) = assets

render :: Assets -> Model -> Picture
render assets model = case running of
  True -> case (length rounds) of
    3 -> Pictures $ baseInterface ++ [userPic (pic playerMove assets), aiPic (pic aiMove assets), winnerPic userScore aiScore]
    _ -> Pictures $ baseInterface ++ [userPic (pic playerMove assets), aiPic (pic aiMove assets)]
  False -> Pictures $ baseInterface
  where
    baseInterface = [title, userText, aiText, aiScorePic aiScore, userScorePic userScore, nbRounds (length rounds)]
    (Model running rounds userScore aiScore) = model
    (Round playerMove aiMove) = head rounds

----------------------------------------------------------------------
--------------------------- View -------------------------------------
----------------------------------------------------------------------

draw :: Model -> View ()
draw model = rws $ \r _ -> ((), (), render r model)

runView :: Assets -> View () -> Picture
runView assets rws' = let ((), (), w) = runRWS rws' assets () in w

----------------------------------------------------------------------
--------------------------- Controller -------------------------------
----------------------------------------------------------------------

inputHandler :: Event -> Controller ()
inputHandler (EventKey (Char 'r') Down _ _) = putState Rock
inputHandler (EventKey (Char 'p') Down _ _) = putState Paper
inputHandler (EventKey (Char 's') Down _ _) = putState Scissors
inputHandler _ = return () -- a

runController :: Controller () -> (Model, StdGen) -> (Model, StdGen)
runController s (m, g) = runRand (execStateT s m) g

-------------------------------------------------------------------

update :: Float -> (Model, StdGen) -> (Model, StdGen)
update _ world = world

-------------------------------------------------------------------

main :: IO ()
main = do
  -- Assets
  rock <- loadBMP "src/assets/rock.bmp"
  paper <- loadBMP "src/assets/paper.bmp"
  scissors <- loadBMP "src/assets/scissors.bmp"
  -- Generator number
  generator <- getStdGen
  -- Parameters
  let display = (InWindow "Rock Paper Scissors" (800, 600) (200, 200))
  let fps = 60
  let world = ((Model False [] 0 0), generator)
  let assets = Assets rock paper scissors
  --
  play display white fps world (\(model, _) -> runView assets (draw model)) (\event -> runController (inputHandler event)) update