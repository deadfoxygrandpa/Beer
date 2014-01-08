module Formatting where

timeDisplay : Time -> String
timeDisplay t = 
    let t' = t * 3600000
        hours = floor <| inHours t'
        minutes = mod (floor <| inMinutes t') 60
        seconds = mod (floor <| inSeconds t') 60
    in String.padLeft 2 '0' (show hours) 
    ++ ":" 
    ++ String.padLeft 2 '0' (show minutes) 
    ++ ":" 
    ++ String.padLeft 2 '0' (show seconds)

peeDisplay : Float -> Bool -> String
peeDisplay urine wetSelf =
    if | wetSelf     -> "you peed your pants, moron. i told you to go to the bathroom"
       | urine < 100 -> "you don't have to pee"
       | urine < 150 -> "you kinda have to pee"
       | urine < 250 -> "you gotta go!"
       | urine < 500 -> "you seriously gotta pee real bad"
       | otherwise   -> "you peed your pants, moron. i told you to go to the bathroom"    