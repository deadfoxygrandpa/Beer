module Main where

-- Standard Library imports
import Window

-- Project imports
import Interface
import Model
import Update
import Constants

initialPerson : Model.Person
initialPerson = Model.Person Model.Male 0 10 0 0 False False

initialState : Model.State
initialState = Model.State initialPerson 0 0 0 0

stateSignal : Signal Model.State
stateSignal = foldp Update.updateState 
                    initialState
                    ( sampleOn (fps Constants.framerate) 
                        ( (\x y z -> Model.Timing x y z) <~ (count Interface.chugClicks) 
                                                          ~ (count Interface.urinateClicks) 
                                                          ~ Interface.timeFactor
                        )
                    )

main : Signal Element
main = Interface.render <~ (count (fps 52)) 
                         ~ (count (fps 43)) 
                         ~ stateSignal 
                         ~ Interface.picker 
                         ~ Window.dimensions