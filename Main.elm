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
import Rendertron

-- Catalog imports
import Generator
import Generator.Standard
import Signal.InputGroups as InputGroups
import Automaton

port title : String
port title = "Beer"

frames : Signal Time
frames = gameGroup.add (fps Constants.framerate) 0

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
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 27)
                          , delay 2 <| Update.closeMenu   <~ (Interface.keyCodePressed 13)
                          , Update.openMenu    <~ Interface.orderClicks2
                          , Update.openMenu    <~ (Interface.keyPressed 'O')
                          ]

gameStateSignal : Signal Model.GameState
gameStateSignal = foldp Update.updateGameState initialGameState gameStateUpdates

gameGroup = InputGroups.makeGroup ((\g -> not . or <| [g.paused, g.menuOpen]) <~ gameStateSignal)
updates' = gameGroup.add updates (Update.emptyFrame 0, 0)

--gameScreen : Signal Element
--gameScreen = Interface.render <~ (sampleOn frames stateSignal)
--                               ~ Window.dimensions
--                               ~ (constant seed)

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

fpsBar : Signal Element
fpsBar = (\w t -> color red <| spacer (clamp 0 w . round <| t / Constants.framerate * (toFloat w)) 2) <~ Window.width ~ feeps

main : Signal Element
main = (\g m x b -> if x then layers [m, b] else layers [g, b])
    <~ gameScreen2 ~ menuScreen ~ (.menuOpen <~ gameStateSignal) ~ fpsBar

-- Rendertrons:

lines : [Rendertron.Rendertron]
lines =
    [ Rendertron.rendertron (\state -> state.messages)
        (\messages -> flow down <| map (plainText . .msg) messages)
        initialState
    , Rendertron.rendertron (\state -> ())
        (\_ -> flow right [Interface.chugButton, plainText " time acceleration: ", flow right Interface.timeAccelerationButtons])
        initialState
    , Rendertron.rendertron (\state -> (state.person.weight, state.person.sex))
        (\(weight, sex) -> flow right [plainText "you are a ", plainText . String.left 5 . show <| weight, plainText "kg ", plainText . show <| sex])
        initialState
    , Rendertron.rendertron (\state -> .name (snd state.person.beers))
        (\name -> flow right [plainText "your current beer of choice is ", plainText . show <| name])
        initialState
    , Rendertron.rendertron (\state -> fst <| state.person.beers)
        (\beer -> flow right [plainText "of which you have ", width 35 <| plainText . show <| beer, plainText " ml left in the glass"])
        initialState
    , Rendertron.rendertron (\state -> state.person.alc)
        (\alc -> flow right [plainText "you got ", plainText . String.left 4 . show <| alc, plainText " grams of unabsorbed ethanol in ur belly"])
        initialState
    , Rendertron.rendertron (\state -> state.person.bac)
        (\bac -> flow right [plainText "ur bac is: ", plainText . String.left 6 . show <| bac])
        initialState
    , Rendertron.rendertron (\state -> (state.person.urine, state.person.wetSelf, state.person.urinating))
        (\(urine, wetSelf, urinating) -> plainText <| Interface.peeDisplay urine wetSelf ++ (if urinating then " (you are peeing)" else ""))
        initialState
    , Rendertron.rendertron (\state -> state.drinks)
        (\drinks -> flow right [plainText "you've had ", plainText . String.left 4 . show <| drinks, plainText " beers"])
        initialState
    ,  Rendertron.rendertron (\state -> Interface.timeDisplay state.elapsed)
        (\elapsed -> flow right [plainText "u been at the bar for: ", plainText elapsed])
        initialState
    ,  Rendertron.rendertron (\state -> ())
        (\_ -> flow right [Interface.sipButton, Interface.gulpButton, Interface.urinateButton])
        initialState
    ,  Rendertron.rendertron (\state -> ())
        (\_ -> flow right [Interface.orderButton, Interface.orderButton2])
        initialState
    ]

gameScreen2 : Signal Element
gameScreen2 = Rendertron.renderGame seed (Rendertron.renderer lines) stateSignal Window.dimensions

feeps = Automaton.run (Automaton.average 30) 0 ((\t -> 1000 / t) <~ (fps Constants.framerate))
