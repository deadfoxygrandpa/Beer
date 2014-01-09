module Interface where

-- Standard Library imports
import Graphics.Input as Input
import String
import Window
import Char
import Keyboard

-- Project imports
import SpecialEffects
import Model
import Constants

makeButtons : String -> (Int, Int) -> [String] -> ([Element], Signal String)
makeButtons initial dimensions names =
    let pool = Input.customButtons initial
    in ( map (\s -> pool.customButton s (box s dimensions white) (box s dimensions lightGrey) (box s dimensions grey)) names
       , pool.events)

keyPressed : Char -> Signal ()
keyPressed char = (\_ -> ()) <~ (dropRepeats <| ((\n -> n `div` 2) <~ (count <| Keyboard.isDown (Char.toCode char))))    

box s (w, h) c = color black . container w h middle 
               . color c . container (w-2) (h-2) middle <| plainText s    

(timeAccelerationButtons, timeAcceleration) = makeButtons "1" (40, 30) ["1", "2", "60", "600"]

(chugButton, chugClicks) = Input.button "slam back a brewski"
(sipButton, sipClicks) = Input.button "sip your beer"
(gulpButton, gulpClicks) = Input.button "gulp down some beer"
(urinateButton, urinateClicks) = Input.button "urinate"
(orderButton, orderClicks) = Input.button "order more beer"
--(picker, timeAcceleration) = Input.stringDropDown <| map show [1..1000]

timeFactor = (\x -> inHours (1000 * (maybe 1 id <| String.toFloat x) / Constants.framerate)) <~ timeAcceleration

instructions : Element
instructions = 
    let controls = flow down <| map plainText [ "spacebar: sip beer"
                                              , "g: gulp some beer"
                                              , "c: chug beer (slam back a brewski)"
                                              , "u: urinate"
                                              , "ctrl + w: win game"
                                              , "clickar buttons: self explanatory" 
                                              ]
    in flow right [ container 100 (heightOf controls) middle <| plainText "controls: "
                  , controls
                  ]

render : Int -> Model.State -> (Int, Int) -> Element
render x state (w, h) = flow outward [instructions,
    SpecialEffects.theBest (Model.Environment (w, h) state (toFloat x)) <|
    flow down [ flow right [chugButton, plainText " time acceleration: ", flow right timeAccelerationButtons]
              , flow right [plainText "you are an ", plainText . show <| state.person.weight, plainText "kg ", plainText . show <| state.person.sex]
              , flow right [plainText "your current beer of choice is ", plainText . show <| (snd state.person.beers).name]
              , flow right [plainText "you got ", plainText . String.left 4 . show <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
              , flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| state.person.bac]
              , plainText <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
              , flow right [plainText "you've had ", plainText . show <| state.drinks, plainText " beers"]
              , flow right [plainText "u been at the bar for: ", plainText <| timeDisplay state.elapsed]
              , urinateButton
              ]]

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