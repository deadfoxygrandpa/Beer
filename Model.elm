module Model where

data Sex = Male | Female

type State = {person : Person, oldBeerClicks : Int, drinks: Int, oldUrineClicks : Int, elapsed : Float}
type Person = {sex : Sex, bac : Float, weight : Float, alc : Float, urine : Float, urinating : Bool, wetSelf : Bool}
type Timing = {beerClicks : Int, urineClicks : Int, timeStep : Float}