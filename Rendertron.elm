module Rendertron where

import Signal
import String

import Model
import Interface
import SpecialEffects

import Automaton (..)
import Automaton

type Rendertron = Automaton Model.State Element
type HiddenState a = {input : a, render : a -> Element, output : Element}

rendertron : (Model.State -> a) -> (a -> Element) -> Model.State -> Rendertron
rendertron getter renderer state =
    let a = getter state
        state' = HiddenState a renderer <| renderer a
        step i s =  let a = getter i
                    in  if a == s.input
                        then (s, s.output)
                        else let o = renderer a
                             in  ({s| input <- a, output <- o}, o)
    in  hiddenState state' step

renderer : [Rendertron] -> Automaton Model.State [Element]
renderer rendertrons = Automaton.combine rendertrons

renderLines : Automaton Model.State [Element] -> Signal Model.State -> Signal Element
renderLines renderer state = flow down <~ run renderer [] state

renderGame : Int -> Automaton Model.State [Element] -> Signal Model.State -> Signal (Int, Int) -> Signal Element
renderGame seed renderer state dimensions =
    let instructions = constant Interface.instructions
        seed' = (\(w, h) -> container w h bottomLeft (plainText <| "random seed: " ++ show seed)) <~ dimensions
        elements = run renderer [spacer 0 0] state
        messages = (\elems (w, h) -> container w h (midTopAt (relative 0.5) (relative 0.7)) <| head elems) <~ elements ~ dimensions
        lines    = (\elems (w, h) -> container w h middle <| flow down (tail elems)) <~ elements ~ dimensions
        distorted = (\(w, h) state messages gameScreen -> layers [messages, gameScreen]) <~ dimensions ~ state ~ messages ~ lines
    in  layers <~ (Signal.combine [instructions, distorted, seed'])

-- Program specific:

lines : Model.State -> [Rendertron]
lines initialState =
    [ rendertron (\state -> state.messages)
        (\messages -> flow down <| map (plainText . .msg) messages)
        initialState
    , rendertron (\state -> ())
        (\_ -> flow right [Interface.chugButton, plainText " time acceleration: ", flow right Interface.timeAccelerationButtons])
        initialState
    , rendertron (\state -> (state.person.weight, state.person.orientation, state.person.gender, state.person.sex, state.person.alcoholism))
        (\(weight, orientation, gender, sex, alcoholism) -> flow right [ plainText "you are a "
                                                                       , plainText . String.left 5 . show <| weight, plainText "kg "
                                                                       , plainText . String.toLower . show <| orientation
                                                                       , plainText <| " " ++ showGender gender sex
                                                                       , plainText . showAlcoholism <| alcoholism
                                                                       ]
        )
        initialState
    , rendertron (\state -> .name (snd state.person.beers))
        (\name -> flow right [plainText "your current beer of choice is ", plainText . show <| name])
        initialState
    , rendertron (\state -> fst <| state.person.beers)
        (\beer -> flow right [plainText "of which you have ", width 35 <| plainText . show <| beer, plainText " ml left in the glass"])
        initialState
    , rendertron (\state -> state.person.alc)
        (\alc -> flow right [plainText "you got ", plainText . String.left 4 . show <| alc, plainText " grams of unabsorbed ethanol in ur belly"])
        initialState
    , rendertron (\state -> state.person.bac)
        (\bac -> flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| bac])
        initialState
    , rendertron (\state -> (state.person.urine, state.person.wetSelf, state.person.urinating))
        (\(urine, wetSelf, urinating) -> plainText <| Interface.peeDisplay urine wetSelf ++ (if urinating then " (you are peeing)" else ""))
        initialState
    , rendertron (\state -> state.drinks)
        (\drinks -> flow right [plainText "you've had ", plainText . String.left 4 . show <| drinks, plainText " beers"])
        initialState
    ,  rendertron (\state -> Interface.timeDisplay state.elapsed)
        (\elapsed -> flow right [plainText "u been at the bar for: ", plainText elapsed])
        initialState
    ,  rendertron (\state -> ())
        (\_ -> flow right [Interface.sipButton, Interface.gulpButton, Interface.urinateButton])
        initialState
    ,  rendertron (\state -> ())
        (\_ -> flow right [Interface.orderButton, Interface.orderButton2])
        initialState
    , rendertron (\state -> (state.person.conscious, state.person.alive))
        (\(conscious, alive) -> if | not alive -> centered . Text.height 30 . bold . toText <| "you are dead. rip"
                                   | not conscious -> centered . Text.height 30 . bold . toText <| "you've passed out"
                                   | otherwise -> spacer 0 0
        )
        initialState
    ]

showGender : Model.Gender -> Model.Sex -> String
showGender gender sex =
    case sex of
        Model.Male   -> if gender == Model.Cis then "cisman" else "transwoman"
        Model.Female -> if gender == Model.Cis then "ciswoman" else "transman"


showAlcoholism : Float -> String
showAlcoholism a = if | a < 0.25  -> " who never drinks"
                      | a < 1     -> " social drinker"
                      | a < 1.5   -> " heavy drinker"
                      | a < 2     -> " functioning alcoholic"
                      | otherwise -> " alcoholic"
