module Rendertron where

import Model

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
