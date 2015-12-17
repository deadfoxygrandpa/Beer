module Rendertron where

import Signal exposing (Signal, map, constant)
import String
import Graphics.Element exposing (Element, container, middle, midTopAt, flow, down, right, bottomLeft, spacer, relative, layers, width, leftAligned, centered)
import List
import Text

import Model
import Interface

import Automaton exposing (..)

type alias Rendertron = Automaton Model.State Element
type alias HiddenState a = {input : a, render : a -> Element, output : Element}

--combine : List (Signal a) -> Signal (List a)
--combine signals =
--    List.foldr (map (::)) (constant []) signals


plainText : String -> Element
plainText = Text.fromString >> leftAligned

head : List a -> a
head list =
    case List.head list of
        Just x -> x
        Nothing -> Debug.crash "head of empty list"

tail : List a -> List a
tail list =
    case List.tail list of
        Just t -> t
        Nothing -> Debug.crash "tail of empty list"

rendertron : (Model.State -> a) -> (a -> Element) -> Model.State -> Rendertron
rendertron getter renderer state =
    let a = getter state
        state' = HiddenState a renderer <| renderer a
        step i s =  let a = getter i
                    in  if a == s.input
                        then (s.output, s)
                        else let o = renderer a
                             in  (o, {s| input = a, output = o})
    in  hiddenState state' step

renderer : List Rendertron -> Automaton Model.State (List Element)
renderer rendertrons = Automaton.combine rendertrons

renderLines : Automaton Model.State (List Element) -> Signal Model.State -> Signal Element
renderLines renderer state = Signal.map (flow down) (run renderer [] state)

renderGame : Int -> Automaton Model.State (List Element) -> Signal Model.State -> Signal (Int, Int) -> Signal Element
renderGame seed renderer state dimensions =
    let instructions = constant Interface.instructions
        seed' = Signal.map (\(w, h) -> container w h bottomLeft (plainText <| "random seed: " ++ toString seed)) dimensions
        elements = run renderer [spacer 0 0] state
        messages = Signal.map2 (\elems (w, h) -> container w h (midTopAt (relative 0.5) (relative 0.7)) <| head elems) elements dimensions
        lines    = Signal.map2 (\elems (w, h) -> container w h middle <| flow down (tail elems)) elements dimensions
        distorted = Signal.map4 (\(w, h) state messages gameScreen -> layers [messages, gameScreen]) dimensions state messages lines
    in  Signal.map layers (Signal.map3 (\a b c -> [a, b, c]) instructions distorted seed')

-- Program specific:

lines : Model.State -> (List Rendertron)
lines initialState =
    [ rendertron (\state -> state.messages)
        (\messages -> flow down <| List.map (plainText << .msg) messages)
        initialState
    , rendertron (\state -> ())
        (\_ -> flow right [Interface.chugButton, plainText " time acceleration: ", flow right Interface.timeAccelerationButtons])
        initialState
    , rendertron (\state -> (state.person.weight, state.person.orientation, state.person.gender, state.person.sex, state.person.alcoholism))
        (\(weight, orientation, gender, sex, alcoholism) -> flow right [ plainText "you are a "
                                                                       , plainText << String.left 5 << toString <| weight, plainText "kg "
                                                                       , plainText << String.toLower << toString <| orientation
                                                                       , plainText <| " " ++ showGender gender sex
                                                                       , plainText << showAlcoholism <| alcoholism
                                                                       ]
        )
        initialState
    , rendertron (\state -> .name (snd state.person.beers))
        (\name -> flow right [plainText "your current beer of choice is ", plainText << toString <| name])
        initialState
    , rendertron (\state -> fst <| state.person.beers)
        (\beer -> flow right [plainText "of which you have ", width 35 <| plainText << toString <| beer, plainText " ml left in the glass"])
        initialState
    , rendertron (\state -> state.person.alc)
        (\alc -> flow right [plainText "you got ", plainText << String.left 4 << toString <| alc, plainText " grams of unabsorbed ethanol in ur belly"])
        initialState
    , rendertron (\state -> state.person.bac)
        (\bac -> flow right [plainText "ur bac is: ", plainText << String.left 6 << toString <| bac])
        initialState
    , rendertron (\state -> (state.person.urine, state.person.wetSelf, state.person.urinating))
        (\(urine, wetSelf, urinating) -> plainText <| Interface.peeDisplay urine wetSelf ++ (if urinating then " (you are peeing)" else ""))
        initialState
    , rendertron (\state -> state.drinks)
        (\drinks -> flow right [plainText "you've had ", plainText << String.left 4 << toString <| drinks, plainText " beers"])
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
        (\(conscious, alive) ->
            if not alive then
                centered << Text.height 30 << Text.bold << Text.fromString <| "you are dead. rip"
            else if not conscious then
                centered << Text.height 30 << Text.bold << Text.fromString <| "you've passed out"
            else
                spacer 0 0
        )
        initialState
    ]

showGender : Model.Gender -> Model.Sex -> String
showGender gender sex =
    case sex of
        Model.Male   -> if gender == Model.Cis then "cisman" else "transwoman"
        Model.Female -> if gender == Model.Cis then "ciswoman" else "transman"


showAlcoholism : Float -> String
showAlcoholism a =
    if a < 0.25 then
        " who never drinks"
    else if a < 1 then
        " social drinker"
    else if a < 1.5 then
        " heavy drinker"
    else if a < 2 then
        " functioning alcoholic"
    else
        " alcoholic"
