module Main where

-- Standard Library imports
import Window
import Keyboard
import Char
import Graphics.Input as Input

-- Project imports
import Interface
import Model
import Update
import Constants
import BeerList
import Randomize
import Menu
import Rendertron
import SpecialEffects

-- Catalog imports
import Generator
import Generator.Standard
import Signal.InputGroups as InputGroups
import Automaton

port title : String
port title = "Beer"

frames : Signal Time
frames = gameGroup.add (fps 5) 0

port seed : Int

gen : Generator.Generator Generator.Standard.Standard
gen = Generator.Standard.generator seed

initialState : Model.State
initialState = Model.State (fst <| Randomize.person gen) 0 0 0 [Model.Message "Bartender: welcome" 5]

time : Signal Time
time = Interface.timeFactor frames

updates : Signal ((Model.State -> Model.State), Time)
updates =
    let step = merges [ Update.sip <~ (keepWhen Keyboard.space 0 time)
                      , Update.sip <~ (sampleOn Interface.sipClicks time)
                      , Update.chug <~ (sampleOn Interface.chugClicks time)
                      , Update.chug <~ (sampleOn (Interface.keyPressed 'C') time)
                      , Update.gulp <~ (keepWhen (Keyboard.isDown <| Char.toCode 'G') 0 time)
                      , Update.gulp <~ (sampleOn Interface.gulpClicks time)
                      , Update.urinate <~ (sampleOn Interface.urinateClicks time)
                      , Update.urinate <~ (sampleOn (Interface.keyPressed 'U') time)
                      , Update.orderAnother <~ Interface.orderClicks
                      , Update.orderAnother <~ (Interface.keyPressed 'A')
                      , Update.order <~ (delay 2 currentBeer)
                      , Update.emptyFrame <~ time
                      ]
    in  (,) <~ step ~ time

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState initialState updates'

initialGameState : Model.GameState
initialGameState = Model.GameState False False

gameStateUpdates : Signal (Model.GameState -> Model.GameState)
gameStateUpdates = merges [ Update.togglePause <~ (Interface.keyPressed 'P')
                          , delay 2 <| Update.closeMenu <~ beerInput.signal
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 27)
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 13)
                          , Update.openMenu    <~ Interface.orderClicks2
                          , Update.openMenu    <~ (Interface.keyPressed 'O')
                          ]

gameStateSignal : Signal Model.GameState
gameStateSignal = foldp Update.updateGameState initialGameState gameStateUpdates

gameGroup = InputGroups.makeGroup ((\g -> not . or <| [g.paused, g.menuOpen]) <~ gameStateSignal)
updates' = gameGroup.add updates (Update.emptyFrame 0, 0)

initialMenu = Menu.menu

menuGroup = InputGroups.makeGroup (.menuOpen <~ gameStateSignal)
menuUpdates' = menuGroup.add menuUpdates id

menuUpdates : Signal (Menu.Menu Model.Beer -> Menu.Menu Model.Beer)
menuUpdates = merges [ Menu.moveUp <~ Interface.keyCodePressed 38
                     , Menu.moveDown <~ Interface.keyCodePressed 40
                     ]

menuSignal : Signal (Menu.Menu Model.Beer)
menuSignal = foldp Menu.update initialMenu menuUpdates'

currentBeer : Signal Model.Beer
currentBeer = merge (Menu.select . .items <~ (sampleOn (menuGroup.add (Interface.keyCodePressed 13) ()) menuSignal))
                    beerInput.signal

beerInput : Input.Input Model.Beer
beerInput = Input.input (snd initialState.person.beers)

quality : Model.Score -> String
quality score = if | score.styleScore > 95 -> " perfect "
                   | score.styleScore > 85 -> "n excellent "
                   | score.styleScore > 75 -> " good "
                   | score.styleScore > 50 -> "n average "
                   | otherwise             -> " terrible "

showBeer : Model.Beer -> String
showBeer beer = beer.name ++ " : a" ++ quality beer.score ++ show beer.style ++ " from " ++ beer.brewery ++ " (" ++ show beer.abv ++ "% ABV)"

menuScreen : Signal Element
menuScreen = Menu.render beerInput showBeer <~ menuSignal ~ Window.dimensions

fpsBar : Signal Element
fpsBar = (\w t -> color red <| spacer (clamp 0 w . round <| t / Constants.framerate * (toFloat w)) 2) <~ Window.width ~ feeps

main : Signal Element
main = (\g m x b dim person t -> let screen = if x then m else g
                    in SpecialEffects.theBest (Model.Environment dim (person.bac / person.alcoholism) <| toFloat t) <| layers [screen, b])
    <~ gameScreen ~ menuScreen ~ (.menuOpen <~ gameStateSignal) ~ fpsBar ~ Window.dimensions ~ (.person <~ stateSignal) ~ (count <| feeps)

gameScreen : Signal Element
gameScreen = Rendertron.renderGame seed (Rendertron.renderer <| Rendertron.lines initialState) stateSignal Window.dimensions

feeps = Automaton.run (Automaton.average 30) 0 ((\t -> 1000 / t) <~ (fps Constants.framerate))
