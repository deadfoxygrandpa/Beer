module Model where

data Sex = Male | Female
data BeerStyle = PaleLager | PremiumBitter

type State = {person : Person, drinks: Float, elapsed : Float}
type Person = {sex : Sex, bac : Float, weight : Float, alc : Float, urine : Float, urinating : Bool, wetSelf : Bool, beers : (Float, Beer)}
type Beer = {name : String, brewery : String, style : BeerStyle, abv : Float, score : Score}
type Score = {overallScore : Float, styleScore : Float}

type Environment = {windowSize : (Int, Int), state : State, time : Float}

type SpecialEffect = Environment -> Element -> Element
