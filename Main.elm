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

frames : Signal Time
frames = fps Constants.framerate

badBeer : Model.Beer
badBeer = BeerList.tsingtao

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 80 0 0 False False (355, badBeer)

initialState : Model.State
initialState = Model.State (fst <| Randomize.makePerson Randomize.gen) 0 0 0 0

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState 
                    initialState
                    ( sampleOn frames
                        ( (\x y z s g -> Model.Timing x y z s g) 
                            <~ (count <| merge Interface.chugClicks (Interface.keyPressed 'C')) 
                             ~ (count <| merge Interface.urinateClicks (Interface.keyPressed 'U'))
                             ~ Interface.timeFactor
                             ~ Keyboard.space
                             ~ (Keyboard.isDown <| Char.toCode 'G')
                        )
                    )

main : Signal Element
main = Interface.render <~ (count frames) 
                         ~ stateSignal 
                         ~ Window.dimensions