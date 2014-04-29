module Rendertron where

import Signal

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
