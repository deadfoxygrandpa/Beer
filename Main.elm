module Main where

-- Standard Library imports
import Window
import Keyboard
import Char
import Random
import Signal exposing (Signal, merge, mergeMany, sampleOn, foldp)
import Time exposing (Time, fps, delay)
import Graphics.Element exposing (Element, color, flow, down, right, spacer, layers)
import Color exposing (red)

-- Project imports
import Interface
import Model
import Update
import Constants
import Randomize
import Menu
import Rendertron
import SpecialEffects

-- Catalog imports
import Automaton
import Signal.Extra exposing (keepWhen, (<~), (~))

-- From timthelion/elm-inputgroups:
type alias InputGroup a =
 {add: Signal a -> a -> Signal a}

makeGroup: Signal Bool -> InputGroup a
makeGroup toggle =
 {add = (\signal a-> keepWhen toggle a signal)}

-- Main program:

port title : String
port title = "Beer"

frames : Signal Time
frames = gameGroup.add (fps 5) 0

seed : Int
seed = 100

gen : Random.Seed
gen = Random.initialSeed seed

initialState : Model.State
initialState = Model.State (fst <| Random.generate Randomize.person gen) 0 0 0 [Model.Message "Bartender: welcome" 5]

time : Signal Time
time = Interface.timeFactor frames

updates : Signal ((Model.State -> Model.State), Time)
updates =
    let step = mergeMany [ Signal.map Update.sip (keepWhen Keyboard.space 0 time)
                         , Signal.map Update.sip (sampleOn Interface.sipClicks time)
                         , Signal.map Update.chug (sampleOn Interface.chugClicks time)
                         , Signal.map Update.chug (sampleOn (Interface.keyPressed 'C') time)
                         , Signal.map Update.gulp (keepWhen (Keyboard.isDown <| Char.toCode 'G') 0 time)
                         , Signal.map Update.gulp (sampleOn Interface.gulpClicks time)
                         , Signal.map Update.urinate (sampleOn Interface.urinateClicks time)
                         , Signal.map Update.urinate (sampleOn (Interface.keyPressed 'U') time)
                         , Signal.map Update.orderAnother Interface.orderClicks
                         , Signal.map Update.orderAnother (Interface.keyPressed 'A')
                         , Signal.map Update.order (delay 2 currentBeer)
                         , Signal.map Update.emptyFrame time
                         ]
    in  Signal.map2 (,) step time

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState initialState updates'

initialGameState : Model.GameState
initialGameState = Model.GameState False False

gameStateUpdates : Signal (Model.GameState -> Model.GameState)
gameStateUpdates = mergeMany [ Signal.map Update.togglePause (Interface.keyPressed 'P')
                             , delay 2 <| Signal.map Update.closeMenu beerInput.signal
                             , delay 2 <| Signal.map Update.closeMenu  (Interface.keyCodePressed 27)
                             , delay 2 <| Signal.map Update.closeMenu  (Interface.keyCodePressed 13)
                             , Signal.map Update.openMenu  Interface.orderClicks2
                             , Signal.map Update.openMenu  (Interface.keyPressed 'O')
                             ]

gameStateSignal : Signal Model.GameState
gameStateSignal = foldp Update.updateGameState initialGameState gameStateUpdates

gameGroup : InputGroup b
gameGroup = makeGroup (Signal.map (\g -> not << (\(a, b) -> a || b) <| (g.paused, g.menuOpen)) gameStateSignal)

updates' : Signal ( Model.State -> Model.State, Float )
updates' = gameGroup.add updates (Update.emptyFrame 0, 0)

initialMenu : Menu.Menu Model.Beer
initialMenu = Menu.menu

menuGroup : InputGroup b
menuGroup = makeGroup (Signal.map .menuOpen gameStateSignal)

menuUpdates' : Signal (Menu.Menu Model.Beer -> Menu.Menu Model.Beer)
menuUpdates' = menuGroup.add menuUpdates identity

menuUpdates : Signal (Menu.Menu Model.Beer -> Menu.Menu Model.Beer)
menuUpdates = mergeMany [ Signal.map Menu.moveUp <| Interface.keyCodePressed 38
                        , Signal.map Menu.moveDown <| Interface.keyCodePressed 40
                        ]

menuSignal : Signal (Menu.Menu Model.Beer)
menuSignal = foldp Menu.update initialMenu menuUpdates'

currentBeer : Signal Model.Beer
currentBeer = merge (Signal.map (Menu.select << .items) (sampleOn (menuGroup.add (Interface.keyCodePressed 13) ()) menuSignal))
                    beerInput.signal

beerInput : Signal.Mailbox Model.Beer
beerInput = Signal.mailbox (snd initialState.person.beers)

quality : Model.Score -> String
quality score =
    if score.styleScore > 95 then
        " perfect "
    else if score.styleScore > 85 then
        "n excellent "
    else if score.styleScore > 75 then
        " good "
    else if score.styleScore > 50 then
        "n average "
    else
        " terrible "

showBeer : Model.Beer -> String
showBeer beer = beer.name ++ " : a" ++ quality beer.score ++ toString beer.style ++ " from " ++ beer.brewery ++ " (" ++ toString beer.abv ++ "% ABV)"

menuScreen : Signal Element
menuScreen = Signal.map2 (Menu.render beerInput showBeer) menuSignal Window.dimensions

fpsBar : Signal Element
fpsBar = Signal.map2 (\w t -> color red <| spacer (clamp 0 w << round <| t / Constants.framerate * (toFloat w)) 2) Window.width feeps

main : Signal Element
main = (\g m x b dim person t -> let screen = if x then m else g
                    in SpecialEffects.theBest (Model.Environment dim (person.bac / person.alcoholism) <| toFloat t) <| layers [screen, b])
    <~ gameScreen ~ menuScreen ~ (.menuOpen <~ gameStateSignal) ~ fpsBar ~ Window.dimensions ~ (.person <~ stateSignal) ~ (count <| feeps)

gameScreen : Signal Element
gameScreen = Rendertron.renderGame seed (Rendertron.renderer <| Rendertron.lines initialState) stateSignal Window.dimensions

feeps : Signal Float
feeps = Automaton.run (Automaton.average 30) 0 ((\t -> 1000 / t) <~ (fps Constants.framerate))

count : Signal a -> Signal Int
count a = Automaton.run Automaton.count 0 a
