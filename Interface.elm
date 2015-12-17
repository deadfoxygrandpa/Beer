module Interface where

-- Standard Library imports
import Graphics.Input as Input
import Signal
import String
import Char
import Keyboard
import List exposing (map, map2, take)
import Color exposing (Color, white, black, lightGrey, grey)
import Graphics.Element exposing (Element, container, middle, color, flow, right, down, heightOf, midTopAt, relative, width, layers, bottomLeft, leftAligned)
import Text exposing (fromString)
import Time exposing (Time, inHours, inMinutes, inSeconds)
import Maybe exposing (Maybe (..), withDefault)
import Result

-- Project imports
import Model

import Signal.Discrete as Discrete

zip : List a -> List b -> List (a, b)
zip = map2 (,)

makeButtons : String -> (Int, Int) -> List String -> (List Element, Signal String)
makeButtons initial dimensions names =
    let inputs = map (\_ -> Signal.mailbox initial) names
        elems  = map (\(i, n) -> Input.customButton (Signal.message i.address n) (box n dimensions white) (box n dimensions lightGrey) (box n dimensions grey)) (zip inputs names)
    in  (elems, Signal.mergeMany <| map .signal inputs)

keyPressed : Char -> Signal ()
keyPressed char = keyCodePressed <| Char.toCode char

keyCodePressed : Char.KeyCode -> Signal ()
keyCodePressed code = Discrete.whenChangeTo True <| Keyboard.isDown code

latch : Signal a -> Signal Bool
latch sig = Discrete.folde not False <| Discrete.whenChange sig

box : String -> (Int, Int) -> Color -> Element
box s (w, h) c = color black << container w h middle
               << color c << container (w-2) (h-2) middle <| (fromString >> leftAligned) s

(timeAccelerationButtons, timeAcceleration) = makeButtons "1" (40, 30) ["1", "2", "60", "600"]

chug = Signal.mailbox ()
sip = Signal.mailbox ()
gulp = Signal.mailbox ()
urinate = Signal.mailbox ()
order = Signal.mailbox ()
order2 = Signal.mailbox ()
(chugButton, chugClicks)       = (Input.button (Signal.message chug.address    ()) "slam back a brewski" , chug.signal)
(sipButton, sipClicks)         = (Input.button (Signal.message sip.address     ()) "sip your beer"       , sip.signal)
(gulpButton, gulpClicks)       = (Input.button (Signal.message gulp.address    ()) "gulp down some beer" , gulp.signal)
(urinateButton, urinateClicks) = (Input.button (Signal.message urinate.address ()) "urinate"             , urinate.signal)
(orderButton, orderClicks)     = (Input.button (Signal.message order.address   ()) "another pint, please", order.signal)
(orderButton2, orderClicks2)   = (Input.button (Signal.message order2.address  ()) "order from the menu" , order2.signal)

timeFactor : Signal Time -> Signal Time
timeFactor time = Signal.map2 (\x t -> inHours (1000 * (withDefault 1 << Result.toMaybe <| String.toFloat x) / (1000 / t))) timeAcceleration time

instructions : Element
instructions =
    let controls = flow down <| map (fromString >> leftAligned) [ "spacebar: sip beer"
                                              , "g: gulp some beer"
                                              , "c: chug beer (slam back a brewski)"
                                              , "u: urinate"
                                              , "a: order another beer"
                                              , "o: order from the menu"
                                              , "ctrl + w: win game"
                                              , "clickar buttons: self explanatory"
                                              , "p: pause game"
                                              ]
    in flow right [ container 100 (heightOf controls) middle <| (fromString >> leftAligned) "controls: "
                  , controls
                  ]

render : Model.State -> (Int, Int) -> Int -> Element
render state (w, h) seed =
    let messages   = container w h (midTopAt (relative 0.5) (relative 0.7)) <| flow down <| map ((fromString >> leftAligned) << .msg) (take 5 state.messages)
        gameScreen = container w h middle <|
                     flow down [ flow right [chugButton, (fromString >> leftAligned) " time acceleration: ", flow right timeAccelerationButtons]
                               , flow right [(fromString >> leftAligned) "you are a ", (fromString >> leftAligned) << String.left 5 << toString <| state.person.weight, (fromString >> leftAligned) "kg ", (fromString >> leftAligned) << toString <| state.person.sex]
                               , flow right [(fromString >> leftAligned) "your current beer of choice is ", (fromString >> leftAligned) << toString <| (snd state.person.beers).name]
                               , flow right [(fromString >> leftAligned) "of which you have ", width 35 <| (fromString >> leftAligned) << toString <| fst state.person.beers, (fromString >> leftAligned) " ml left in the glass"]
                               , flow right [(fromString >> leftAligned) "you got ", (fromString >> leftAligned) << String.left 4 << toString <| state.person.alc, (fromString >> leftAligned) " grams of unabsorbed ethanol in ur belly"]
                               , flow right [(fromString >> leftAligned) "ur bac is: ", (fromString >> leftAligned) << String.left 6 << toString <| state.person.bac]
                               , (fromString >> leftAligned) <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
                               , flow right [(fromString >> leftAligned) "you've had ", (fromString >> leftAligned) << String.left 4 << toString <| state.drinks, (fromString >> leftAligned) " beers"]
                               , flow right [(fromString >> leftAligned) "u been at the bar for: ", (fromString >> leftAligned) <| timeDisplay state.elapsed]
                               , flow right [sipButton, gulpButton, urinateButton]
                               , flow right [orderButton, orderButton2]
                               ]
        distorted = layers [messages, gameScreen]
    in  layers [instructions, distorted, container w h bottomLeft ((fromString >> leftAligned) <| "random seed: " ++ toString seed)]

gameScreen : Model.State -> (Int, Int) -> Element
gameScreen state (w, h) = container w h middle <|
    flow down [ flow right [chugButton, (fromString >> leftAligned) " time acceleration: ", flow right timeAccelerationButtons]
              , flow right [(fromString >> leftAligned) "you are an ", (fromString >> leftAligned) << toString <| state.person.weight, (fromString >> leftAligned) "kg ", (fromString >> leftAligned) << toString <| state.person.sex]
              , flow right [(fromString >> leftAligned) "your current beer of choice is ", (fromString >> leftAligned) << toString <| (snd state.person.beers).name]
              , flow right [(fromString >> leftAligned) "of which you have ", (fromString >> leftAligned) << toString <| fst state.person.beers, (fromString >> leftAligned) " ml left in the glass"]
              , flow right [(fromString >> leftAligned) "you got ", (fromString >> leftAligned) << String.left 4 << toString <| state.person.alc, (fromString >> leftAligned) " grams of unabsorbed ethanol in ur belly"]
              , flow right [(fromString >> leftAligned) "ur bac is: ", (fromString >> leftAligned) << String.left 6 << toString <| state.person.bac]
              , (fromString >> leftAligned) <| peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
              , flow right [(fromString >> leftAligned) "you've had ", (fromString >> leftAligned) << toString <| state.drinks, (fromString >> leftAligned) " beers"]
              , flow right [(fromString >> leftAligned) "u been at the bar for: ", (fromString >> leftAligned) <| timeDisplay state.elapsed]
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
    if wetSelf then
      "you peed your pants, moron. i told you to go to the bathroom"
    else if urine < 100 then
      "you don't have to pee"
    else if urine < 150 then
      "you kinda have to pee"
    else if urine < 250 then
      "you gotta go!"
    else if urine < 500 then
      "you seriously gotta pee real bad"
    else
      "you peed your pants, moron. i told you to go to the bathroom"
