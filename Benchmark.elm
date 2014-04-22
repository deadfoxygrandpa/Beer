module Benchmark where

-- Project imports
import Interface
import Model
import Update
import Constants
import BeerList
import Randomize
import Menu
import SpecialEffects
import Rendertron

-- Catalog imports
import Generator
import Generator.Standard
import Signal.InputGroups as InputGroups
import Automaton

discard : a -> ()
discard _ = ()

badBeer : Model.Beer
badBeer = BeerList.tsingtao

seed = 10

gen : Generator.Generator Generator.Standard.Standard
gen = Generator.Standard.generator seed

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 80 0 0 False False (355, badBeer)

initialState : Model.State
initialState = Model.State (fst <| Randomize.person gen) 0 0 0 [Model.Message "Bartender: welcome" 5]

initialGameState : Model.GameState
initialGameState = Model.GameState False False

initialMenu : Menu.Menu Model.Beer
initialMenu = Menu.menu

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
    ,  Rendertron.rendertron (\state -> state.elapsed)
        (\elapsed -> flow right [plainText "u been at the bar for: ", plainText <| Interface.timeDisplay elapsed])
        initialState
    ,  Rendertron.rendertron (\state -> ())
        (\_ -> flow right [Interface.sipButton, Interface.gulpButton, Interface.urinateButton])
        initialState
    ,  Rendertron.rendertron (\state -> ())
        (\_ -> flow right [Interface.orderButton, Interface.orderButton2])
        initialState
    ]

renderer = Rendertron.renderer lines

step5 : Automaton.Automaton i o -> [i] -> o
step5 aut is =
    let (aut1, is1) = (fst <| Automaton.step (head is) aut, tail is)
        (aut2, is2) = (fst <| Automaton.step (head is1) aut1, tail is1)
        (aut3, is3) = (fst <| Automaton.step (head is2) aut2, tail is2)
        (aut4, is4) = (fst <| Automaton.step (head is3) aut3, tail is3)
        (aut5, o) = Automaton.step (head is4) aut4
    in o

port rendertron : () -> ()
port rendertron = \_ -> discard <| step5 renderer (map (\x -> {initialState| elapsed <- x}) [0..5])

port rendertron2 : () -> ()
port rendertron2 = \_ -> discard <| step5 renderer (map (\x -> {initialState| elapsed <- x}) (repeat 5 0))

port rendertronnop : () -> ()
port rendertronnop = \_ -> discard <| map (\x -> Interface.gameScreen initialState (x, x)) (repeat 5 100)

port sip : () -> ()
port sip = \_ -> discard <| Update.updateState ((Update.sip 5), 5) initialState

port chug : () -> ()
port chug = \_ -> discard <| Update.updateState ((Update.chug 5), 5) initialState

port gulp : () -> ()
port gulp = \_ -> discard <| Update.updateState ((Update.gulp 5), 5) initialState

port urinate : () -> ()
port urinate = \_ -> discard <| Update.updateState ((Update.urinate 5), 5) initialState

port orderAnother : () -> ()
port orderAnother = \_ -> discard <| Update.updateState ((Update.orderAnother 5), 5) initialState

port order : () -> ()
port order = \_ -> discard <| Update.updateState ((Update.order BeerList.tsingtao), 5) initialState

port emptyFrame : () -> ()
port emptyFrame = \_ -> discard <| Update.updateState ((Update.emptyFrame 5), 5) initialState

port togglePause : () -> ()
port togglePause = \_ -> discard <| Update.updateGameState (Update.togglePause 5) initialGameState

port closeMenu : () -> ()
port closeMenu = \_ -> discard <| Update.updateGameState (Update.closeMenu 5) initialGameState

port openMenu : () -> ()
port openMenu = \_ -> discard <| Update.updateGameState (Update.openMenu 5) initialGameState

port moveUp : () -> ()
port moveUp = \_ -> discard <| Menu.update (Menu.moveUp 5) initialMenu

port moveDown : () -> ()
port moveDown = \_ -> discard <| Menu.update (Menu.moveDown 5) initialMenu

port render : () -> ()
port render = \_ -> discard <| Interface.render initialState (500, 500) 500

port menuRender : () -> ()
port menuRender = \_ -> discard <| Menu.render .name initialMenu (500, 500)

port theBest : () -> ()
port theBest = \_ -> discard <| SpecialEffects.theBest (Model.Environment (500, 500) initialState (toFloat initialState.frames))

port gameScreen : () -> ()
port gameScreen = \_ -> discard <| Interface.gameScreen initialState (500, 500)
