module Model where

data Sex = Male | Female

type State = {person : Person, oldBeerClicks : Int, drinks: Int, oldUrineClicks : Int, elapsed : Float}
type Person = {sex : Sex, bac : Float, weight : Float, alc : Float, urine : Float, urinating : Bool, wetSelf : Bool, beers : (Float, Beer)}
type Beer = {name : String, brewery : String, style : String, abv : Float, score : Score}
type Score = {overallScore : Float, styleScore : Float}

type Timing = {beerClicks : Int, urineClicks : Int, timeStep : Float}
type Environment = {windowSize : (Int, Int), state : State}