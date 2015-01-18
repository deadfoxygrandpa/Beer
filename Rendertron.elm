module Rendertron where

import Signal (Signal, (<~), (~), map, constant)
import String
import Graphics.Element (Element, container, middle, midTopAt, flow, down, right, bottomLeft, spacer, relative, layers, width)
import List
import Text

import Model
import Interface
import SpecialEffects

import Automaton (..)
import Automaton

type alias Rendertron = Automaton Model.State Element
type alias HiddenState a = {input : a, render : a -> Element, output : Element}

--combine : List (Signal a) -> Signal (List a)
--combine signals =
--    List.foldr (map (::)) (constant []) signals

rendertron : (Model.State -> a) -> (a -> Element) -> Model.State -> Rendertron
rendertron getter renderer state =
    let a = getter state
        state' = HiddenState a renderer <| renderer a
        step i s =  let a = getter i
                    in  if a == s.input
                        then (s.output, s)
                        else let o = renderer a
                             in  (o, {s| input <- a, output <- o})
    in  hiddenState state' step

renderer : List Rendertron -> Automaton Model.State (List Element)
renderer rendertrons = Automaton.combine rendertrons

renderLines : Automaton Model.State (List Element) -> Signal Model.State -> Signal Element
renderLines renderer state = flow down <~ run renderer [] state

renderGame : Int -> Automaton Model.State (List Element) -> Signal Model.State -> Signal (Int, Int) -> Signal Element
renderGame seed renderer state dimensions =
    let instructions = constant Interface.instructions
        seed' = (\(w, h) -> container w h bottomLeft (Text.plainText <| "random seed: " ++ toString seed)) <~ dimensions
        elements = run renderer [spacer 0 0] state
        messages = (\elems (w, h) -> container w h (midTopAt (relative 0.5) (relative 0.7)) <| List.head elems) <~ elements ~ dimensions
        lines    = (\elems (w, h) -> container w h middle <| flow down (List.tail elems)) <~ elements ~ dimensions
        distorted = (\(w, h) state messages gameScreen -> layers [messages, gameScreen]) <~ dimensions ~ state ~ messages ~ lines
    in  layers <~ ((\a b c -> [a, b, c]) <~ instructions ~ distorted ~ seed')

-- Program specific:

lines : Model.State -> (List Rendertron)
lines initialState =
    [ rendertron (\state -> state.messages)
        (\messages -> flow down <| List.map (Text.plainText << .msg) messages)
        initialState
    , rendertron (\state -> ())
        (\_ -> flow right [Interface.chugButton, Text.plainText " time acceleration: ", flow right Interface.timeAccelerationButtons])
        initialState
    , rendertron (\state -> (state.person.weight, state.person.orientation, state.person.gender, state.person.sex, state.person.alcoholism))
        (\(weight, orientation, gender, sex, alcoholism) -> flow right [ Text.plainText "you are a "
                                                                       , Text.plainText << String.left 5 << toString <| weight, Text.plainText "kg "
                                                                       , Text.plainText << String.toLower << toString <| orientation
                                                                       , Text.plainText <| " " ++ showGender gender sex
                                                                       , Text.plainText << showAlcoholism <| alcoholism
                                                                       ]
        )
        initialState
    , rendertron (\state -> .name (snd state.person.beers))
        (\name -> flow right [Text.plainText "your current beer of choice is ", Text.plainText << toString <| name])
        initialState
    , rendertron (\state -> fst <| state.person.beers)
        (\beer -> flow right [Text.plainText "of which you have ", width 35 <| Text.plainText << toString <| beer, Text.plainText " ml left in the glass"])
        initialState
    , rendertron (\state -> state.person.alc)
        (\alc -> flow right [Text.plainText "you got ", Text.plainText << String.left 4 << toString <| alc, Text.plainText " grams of unabsorbed ethanol in ur belly"])
        initialState
    , rendertron (\state -> state.person.bac)
        (\bac -> flow right [Text.plainText "ur bac is: ", Text.plainText << String.left 6 << toString <| bac])
        initialState
    , rendertron (\state -> (state.person.urine, state.person.wetSelf, state.person.urinating))
        (\(urine, wetSelf, urinating) -> Text.plainText <| Interface.peeDisplay urine wetSelf ++ (if urinating then " (you are peeing)" else ""))
        initialState
    , rendertron (\state -> state.drinks)
        (\drinks -> flow right [Text.plainText "you've had ", Text.plainText << String.left 4 << toString <| drinks, Text.plainText " beers"])
        initialState
    ,  rendertron (\state -> Interface.timeDisplay state.elapsed)
        (\elapsed -> flow right [Text.plainText "u been at the bar for: ", Text.plainText elapsed])
        initialState
    ,  rendertron (\state -> ())
        (\_ -> flow right [Interface.sipButton, Interface.gulpButton, Interface.urinateButton])
        initialState
    ,  rendertron (\state -> ())
        (\_ -> flow right [Interface.orderButton, Interface.orderButton2])
        initialState
    , rendertron (\state -> (state.person.conscious, state.person.alive))
        (\(conscious, alive) -> if | not alive -> Text.centered << Text.height 30 << Text.bold << Text.fromString <| "you are dead. rip"
                                   | not conscious -> Text.centered << Text.height 30 << Text.bold << Text.fromString <| "you've passed out"
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
