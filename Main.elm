module Main where

-- Standard Library imports
import Window
import Keyboard
import Char

-- Project imports
import Interface
import Model
import Update
import Constants
import BeerList
import Randomize
import Menu

-- Catalog imports
import Generator
import Generator.Standard
import Signal.InputGroups as InputGroups

frames : Signal Time
frames = fps Constants.framerate

badBeer : Model.Beer
badBeer = BeerList.tsingtao

port seed : Int

gen : Generator.Generator Generator.Standard.Standard
gen = Generator.Standard.generator seed

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 80 0 0 False False (355, badBeer)

initialState : Model.State
initialState = Model.State (fst <| Randomize.person gen) 0 0 0 [Model.Message "Bartender: welcome" 5]

time : Signal Time
time = sampleOn frames Interface.timeFactor

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
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 27)
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 13)
                          , Update.openMenu    <~ Interface.orderClicks2
                          , Update.openMenu    <~ (Interface.keyPressed 'O')
                          ]

gameStateSignal : Signal Model.GameState
gameStateSignal = foldp Update.updateGameState initialGameState gameStateUpdates

gameGroup = InputGroups.makeGroup ((\g -> not . or <| [g.paused, g.menuOpen]) <~ gameStateSignal)
updates' = gameGroup.add updates (Update.emptyFrame 0, 0)

gameScreen : Signal Element
gameScreen = Interface.render <~ (sampleOn frames stateSignal)
                               ~ Window.dimensions
                               ~ (constant seed)

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
currentBeer = Menu.select . .items <~ (sampleOn (menuGroup.add (Interface.keyCodePressed 13) ()) menuSignal)

menuScreen : Signal Element
menuScreen = Menu.render .name <~ menuSignal ~ Window.dimensions

main : Signal Element
main = (\g m x -> if x then m else g) <~ gameScreen ~ menuScreen ~ (.menuOpen <~ gameStateSignal)
