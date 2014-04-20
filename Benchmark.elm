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


port rendertron : () -> ()
port rendertron = \_ -> discard <| Automaton.step Rendertron.state Rendertron.renderer

port rendertron1 : () -> ()
port rendertron1 = \_ -> discard <| Automaton.step (Rendertron.State "name" "") Rendertron.renderer

port rendertron2 : () -> ()
port rendertron2 = \_ -> discard <| Automaton.step (Rendertron.State "" "") Rendertron.renderer

port rendertron3 : () -> ()
port rendertron3 = \_ -> discard <| flow down [plainText "name", plainText "content"]

port rendertron4 : () -> ()
port rendertron4 = \_ -> discard <| flow down [plainText "", plainText ""]

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
