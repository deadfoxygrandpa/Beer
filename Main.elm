module Main where

-- Standard Library imports
import Window
import Keyboard

-- Project imports
import Interface
import Model
import Update
import Constants
import BeerList

frames : Signal Time
frames = fps Constants.framerate

badBeer : Model.Beer
badBeer = BeerList.budweiser

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 10 0 0 False False (1, badBeer)

initialState : Model.State
initialState = Model.State initialPerson 0 0 0 0

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState 
                    initialState
                    ( sampleOn frames
                        ( (\x y z s -> Model.Timing x y z s) <~ (count <| merge Interface.chugClicks (Interface.keyPressed 'C')) 
                                                              ~ (count <| merge Interface.urinateClicks (Interface.keyPressed 'U'))
                                                              ~ Interface.timeFactor
                                                              ~ Keyboard.space
                        )
                    )

main : Signal Element
main = Interface.render <~ (count frames) 
                         ~ stateSignal 
                         ~ Window.dimensions