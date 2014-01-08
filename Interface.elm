module Interface where

-- Standard Library imports
import Graphics.Input as Input
import String
import Window

-- Project imports
import SpecialEffects
import Model
import Formatting
import Update
import Constants

(chugButton, chugClicks) = Input.button "slam back a brewski"
(urinateButton, urinateClicks) = Input.button "urinate"
(picker, timeAcceleration) = Input.stringDropDown <| map show [1..1000]
timeFactor = (\x -> inHours (1000 * (maybe 1 id <| String.toFloat x) / Constants.framerate)) <~ timeAcceleration

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 10 0 0 False False

initialState : Model.State
initialState = Model.State initialPerson 0 0 0 0

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState initialState (sampleOn (fps Constants.framerate) ((\x y z -> Model.Timing x y z) <~ (count chugClicks) ~ (count urinateClicks) ~ timeFactor))

scene : Int -> Int -> Model.State -> Element -> (Int, Int) -> Element
scene x y state picka (w, h) = SpecialEffects.distort w h (toFloat x) (toFloat y) state.person.bac state.person.bac <|
    flow down [ flow right [chugButton, plainText " time acceleration: ", picka]
              , flow right [plainText "you are an ", plainText . show <| state.person.weight, plainText "kg ", plainText . show <| state.person.sex]
              , flow right [plainText "you got ", plainText . String.left 4 . show <| state.person.alc, plainText " grams of unabsorbed ethanol in ur belly"]
              , flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| state.person.bac]
              , plainText <| Formatting.peeDisplay state.person.urine state.person.wetSelf ++ (if state.person.urinating then " (you are peeing)" else "")
              , flow right [plainText "you've had ", plainText . show <| state.drinks, plainText " beers"]
              , flow right [plainText "u been at the bar for: ", plainText <| Formatting.timeDisplay state.elapsed]
              , urinateButton
              ]

main : Signal Element
main = scene <~ (count (fps 52)) ~ (count (fps 43)) ~ stateSignal ~ picker ~ Window.dimensions