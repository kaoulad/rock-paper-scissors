module Game.Pictures where

import Graphics.Gloss

---------------------------------

title :: Picture
title = Scale 0.5 0.5 (Translate (-700) 410 (Text "Rock Paper Scissors"))

resultMsg :: String -> Picture
resultMsg msg = Scale 0.2 0.2 (Translate (-600) 700 (Text msg))

winnerPic :: Int -> Int -> Picture
winnerPic userScore aiScore
          | userScore == aiScore = resultMsg "Equality !"
          | userScore > aiScore = Color green $ resultMsg "You won !"
          | otherwise = Color red $ resultMsg "You lost !"

nbRounds :: Int -> Picture
nbRounds n = Scale 0.2 0.2 (Translate 1000 700 (Text $ "Round: "++(show n)))

---------------------------------

userText :: Picture
userText = Scale 0.15 0.15 (Translate (-1200) 100 (Text "User"))

aiText :: Picture
aiText = Scale 0.15 0.15 (Translate (-1200) (-1300) (Text "CPU"))

---------------------------------

userPic :: Picture -> Picture
userPic pic = Translate 200 20 pic

aiPic :: Picture -> Picture
aiPic pic = Translate 200 (-200) pic

---------------------------------

userScorePic :: Int -> Picture
userScorePic score = Color blue $ Scale 0.18 0.18 (Translate (-1250) 80 (Text (show score)))

aiScorePic :: Int -> Picture
aiScorePic score = Color red $ Scale 0.18 0.18 (Translate (-1250) (-1100) (Text (show score)))

---------------------------------