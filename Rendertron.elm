module Rendertron where

import Automaton (..)
import Automaton

type State = {name : String, content : String}
type HiddenState a = {input : a, output : Element}

renderName : Automaton State Element
renderName = hiddenState (HiddenState "" <| plainText "") (\input state -> if input.name == state.input then (state, state.output) else let x = plainText input.name in (HiddenState input.name <| x, x))

renderContent : Automaton State Element
renderContent = hiddenState (HiddenState "" <| plainText "") (\input state -> if input.content == state.input then (state, state.output) else let x = plainText input.content in (HiddenState input.content <| x, x))

renderer : Automaton State [Element]
renderer = Automaton.combine [renderName, renderContent]

render : Signal State -> Signal Element
render state = flow down <~ (run renderer [spacer 0 0] state)

state : State
state = State "name" "content"

main = (sampleOn (fps 1000) (constant <| plainText "welp,"))
