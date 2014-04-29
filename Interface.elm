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
    let inputs = map (\_ -> Input.input initial) names
        elems  = map (\(i, n) -> Input.customButton i.handle n (box n dimensions white) (box n dimensions lightGrey) (box n dimensions grey)) (zip inputs names)
    in  (elems, merges <| map .signal inputs)

keyPressed : Char -> Signal ()
keyPressed char = keyCodePressed <| Char.toCode char

keyCodePressed : Char.KeyCode -> Signal ()
keyCodePressed code = (\_ -> ()) <~ (dropRepeats <| ((\n -> n `div` 2) <~ (count <| Keyboard.isDown code)))

latch : Signal a -> Signal Bool
latch sig = dropRepeats <| (\x -> mod x 2 == 0) <~ (count sig)

box : String -> (Int, Int) -> Color -> Element
box s (w, h) c = color black . container w h middle
               . color c . container (w-2) (h-2) middle <| plainText s

(timeAccelerationButtons, timeAcceleration) = makeButtons "1" (40, 30) ["1", "2", "60", "600"]

chug = Input.input ()
sip = Input.input ()
gulp = Input.input ()
urinate = Input.input ()
order = Input.input ()
order2 = Input.input ()
(chugButton, chugClicks)       = (Input.button chug.handle    () "slam back a brewski" , chug.signal)
(sipButton, sipClicks)         = (Input.button sip.handle     () "sip your beer"       , sip.signal)
(gulpButton, gulpClicks)       = (Input.button gulp.handle    () "gulp down some beer" , gulp.signal)
(urinateButton, urinateClicks) = (Input.button urinate.handle () "urinate"             , urinate.signal)
(orderButton, orderClicks)     = (Input.button order.handle   () "another pint, please", order.signal)
(orderButton2, orderClicks2)   = (Input.button order2.handle  () "order from the menu" , order2.signal)

timeFactor : Signal Time -> Signal Time
timeFactor time = (\x t -> inHours (1000 * (maybe 1 id <| String.toFloat x) / (1000 / t))) <~ timeAcceleration ~ time

instructions : Element
instructions =
    let controls = flow down <| map plainText [ "spacebar: sip beer"
                                              , "g: gulp some beer"
                                              , "c: chug beer (slam back a brewski)"
                                              , "u: urinate"
                                              , "a: order another beer"
                                              , "o: order from the menu"
                                              , "ctrl + w: win game"
                                              , "clickar buttons: self explanatory"
                                              , "p: pause game"
                                              ]
    in flow right [ container 100 (heightOf controls) middle <| plainText "controls: "
                  , controls
                  ]

render : Model.State -> (Int, Int) -> Int -> Element
render state (w, h) seed =
    let messages   = container w h (midTopAt (relative 0.5) (relative 0.7)) <| flow down <| map (plainText . .msg) (take 5 state.messages)
        gameScreen = container w h middle <|
                     flow down [ flow right [chugButton, plainText " time acceleration: ", flow right timeAccelerationButtons]
                               , flow right [plainText "you are a ", plainText . String.left 5 . show <| state.person.weight, plainText "kg ", plainText . show <| state.person.sex]
                               , flow right [plainText "your current beer of choice is ", plainText . show <| (snd state.person.beers).name]
                               , flow right [plainText "of which you have ", width 35 <| plainText . show <| fst state.person.beers, plainText " ml left in the glass"]
                               , flow right [plainText "you got ", plainText . String.left 4 . show <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
                               , flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| state.person.bac]
                               , plainText <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
                               , flow right [plainText "you've had ", plainText . String.left 4 . show <| state.drinks, plainText " beers"]
                               , flow right [plainText "u been at the bar for: ", plainText <| timeDisplay state.elapsed]
                               , flow right [sipButton, gulpButton, urinateButton]
                               , flow right [orderButton, orderButton2]
                               ]
        distorted = layers [messages, gameScreen]
    in  layers [instructions, distorted, container w h bottomLeft (plainText <| "random seed: " ++ show seed)]

gameScreen : Model.State -> (Int, Int) -> Element
gameScreen state (w, h) = container w h middle <|
    flow down [ flow right [chugButton, plainText " time acceleration: ", flow right timeAccelerationButtons]
              , flow right [plainText "you are an ", plainText . show <| state.person.weight, plainText "kg ", plainText . show <| state.person.sex]
              , flow right [plainText "your current beer of choice is ", plainText . show <| (snd state.person.beers).name]
              , flow right [plainText "of which you have ", plainText . show <| fst state.person.beers, plainText " ml left in the glass"]
              , flow right [plainText "you got ", plainText . String.left 4 . show <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
              , flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| state.person.bac]
              , plainText <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
              , flow right [plainText "you've had ", plainText . show <| state.drinks, plainText " beers"]
              , flow right [plainText "u been at the bar for: ", plainText <| timeDisplay state.elapsed]
              , flow right [sipButton, gulpButton, urinateButton]
              , flow right [orderButton, orderButton2]
              ]

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
