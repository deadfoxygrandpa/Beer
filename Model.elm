module Model where

data Sex = Male | Female
data BeerStyle = PaleLager | PremiumBitter

type State = {person : Person, drinks: Float, elapsed : Float, frames : Int, messages : [Message]}
type Person = { sex : Sex
              , bac : Float
              , weight : Float
              , alc : Float
              , urine : Float
              , urinating : Bool
              , wetSelf : Bool
              , beers : (Float, Beer)
              , alcoholism : Float
              , conscious : Bool
              , alive : Bool
              }
type Beer = {name : String, brewery : String, style : BeerStyle, abv : Float, score : Score}
type Score = {overallScore : Float, styleScore : Float}

type Environment = {windowSize : (Int, Int), factor : Float, time : Float}

type SpecialEffect = Environment -> Element -> Element

type GameState = {paused : Bool, menuOpen : Bool}

type Message = {msg : String, timeout : Time}
