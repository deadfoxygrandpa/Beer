module Interface where

-- Standard Library imports
import Graphics.Input as Input
import Signal
import String
import Window
import Char
import Keyboard
import List (map, map2, take)
import Color (Color, white, black, lightGrey, grey)
import Graphics.Element (Element, container, middle, color, flow, right, down, heightOf, midTopAt, relative, width, layers, bottomLeft)
import Text (plainText)
import Time (Time, inHours, inMinutes, inSeconds)
import Maybe (Maybe (..), withDefault)

-- Project imports
import SpecialEffects
import Model
import Constants

import Signal.Discrete as Discrete

zip = map2 (,)

makeButtons : String -> (Int, Int) -> List String -> (List Element, Signal String)
makeButtons initial dimensions names =
    let inputs = map (\_ -> Signal.channel initial) names
        elems  = map (\(i, n) -> Input.customButton (Signal.send i n) (box n dimensions white) (box n dimensions lightGrey) (box n dimensions grey)) (zip inputs names)
    in  (elems, Signal.mergeMany <| map Signal.subscribe inputs)

keyPressed : Char -> Signal ()
keyPressed char = keyCodePressed <| Char.toCode char

keyCodePressed : Char.KeyCode -> Signal ()
keyCodePressed code = Discrete.whenChangeTo True <| Keyboard.isDown code

latch : Signal a -> Signal Bool
latch sig = Discrete.folde not False <| Discrete.whenChange sig

box : String -> (Int, Int) -> Color -> Element
box s (w, h) c = color black << container w h middle
               << color c << container (w-2) (h-2) middle <| plainText s

(timeAccelerationButtons, timeAcceleration) = makeButtons "1" (40, 30) ["1", "2", "60", "600"]

chug = Signal.channel ()
sip = Signal.channel ()
gulp = Signal.channel ()
urinate = Signal.channel ()
order = Signal.channel ()
order2 = Signal.channel ()
(chugButton, chugClicks)       = (Input.button (Signal.send chug    ()) "slam back a brewski" , Signal.subscribe chug)
(sipButton, sipClicks)         = (Input.button (Signal.send sip     ()) "sip your beer"       , Signal.subscribe sip)
(gulpButton, gulpClicks)       = (Input.button (Signal.send gulp    ()) "gulp down some beer" , Signal.subscribe gulp)
(urinateButton, urinateClicks) = (Input.button (Signal.send urinate ()) "urinate"             , Signal.subscribe urinate)
(orderButton, orderClicks)     = (Input.button (Signal.send order   ()) "another pint, please", Signal.subscribe order)
(orderButton2, orderClicks2)   = (Input.button (Signal.send order2  ()) "order from the menu" , Signal.subscribe order2)

timeFactor : Signal Time -> Signal Time
timeFactor time = Signal.map2 (\x t -> inHours (1000 * (withDefault 1 <| String.toFloat x) / (1000 / t))) timeAcceleration time

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
    let messages   = container w h (midTopAt (relative 0.5) (relative 0.7)) <| flow down <| map (plainText << .msg) (take 5 state.messages)
        gameScreen = container w h middle <|
                     flow down [ flow right [chugButton, plainText " time acceleration: ", flow right timeAccelerationButtons]
                               , flow right [plainText "you are a ", plainText << String.left 5 << toString <| state.person.weight, plainText "kg ", plainText << toString <| state.person.sex]
                               , flow right [plainText "your current beer of choice is ", plainText << toString <| (snd state.person.beers).name]
                               , flow right [plainText "of which you have ", width 35 <| plainText << toString <| fst state.person.beers, plainText " ml left in the glass"]
                               , flow right [plainText "you got ", plainText << String.left 4 << toString <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
                               , flow right [plainText "ur bac is: ", plainText << String.left 6 << toString <| state.person.bac]
                               , plainText <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
                               , flow right [plainText "you've had ", plainText << String.left 4 << toString <| state.drinks, plainText " beers"]
                               , flow right [plainText "u been at the bar for: ", plainText <| timeDisplay state.elapsed]
                               , flow right [sipButton, gulpButton, urinateButton]
                               , flow right [orderButton, orderButton2]
                               ]
        distorted = layers [messages, gameScreen]
    in  layers [instructions, distorted, container w h bottomLeft (plainText <| "random seed: " ++ toString seed)]

gameScreen : Model.State -> (Int, Int) -> Element
gameScreen state (w, h) = container w h middle <|
    flow down [ flow right [chugButton, plainText " time acceleration: ", flow right timeAccelerationButtons]
              , flow right [plainText "you are an ", plainText << toString <| state.person.weight, plainText "kg ", plainText << toString <| state.person.sex]
              , flow right [plainText "your current beer of choice is ", plainText << toString <| (snd state.person.beers).name]
              , flow right [plainText "of which you have ", plainText << toString <| fst state.person.beers, plainText " ml left in the glass"]
              , flow right [plainText "you got ", plainText << String.left 4 << toString <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
              , flow right [plainText "ur bac is: ", plainText << String.left 6 << toString <| state.person.bac]
              , plainText <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
              , flow right [plainText "you've had ", plainText << toString <| state.drinks, plainText " beers"]
              , flow right [plainText "u been at the bar for: ", plainText <| timeDisplay state.elapsed]
              , flow right [sipButton, gulpButton, urinateButton]
              , flow right [orderButton, orderButton2]
              ]

timeDisplay : Time -> String
timeDisplay t =
    let t' = t * 3600000
        hours = floor <| inHours t'
        minutes = (floor <| inMinutes t') % 60
        seconds = (floor <| inSeconds t') % 60
    in String.padLeft 2 '0' (toString hours)
    ++ ":"
    ++ String.padLeft 2 '0' (toString minutes)
    ++ ":"
    ++ String.padLeft 2 '0' (toString seconds)

peeDisplay : Float -> Bool -> String
peeDisplay urine wetSelf =
    if | wetSelf     -> "you peed your pants, moron. i told you to go to the bathroom"
       | urine < 100 -> "you don't have to pee"
       | urine < 150 -> "you kinda have to pee"
       | urine < 250 -> "you gotta go!"
       | urine < 500 -> "you seriously gotta pee real bad"
       | otherwise   -> "you peed your pants, moron. i told you to go to the bathroom"
