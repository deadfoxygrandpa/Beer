module Model where

import Time exposing (Time)
import Graphics.Element exposing (Element)

type Sex = Male | Female
type Gender = Cis | Trans
type Orientation = Straight | Bisexual | Gay | Pansexual | Asexual
type BeerStyle = PaleLager | PremiumBitter

type alias State = {person : Person, drinks: Float, elapsed : Float, frames : Int, messages : List Message}
type alias Person = { sex : Sex
                    , gender : Gender
                    , orientation : Orientation
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

type alias Beer = {name : String, brewery : String, style : BeerStyle, abv : Float, score : Score}
type alias Score = {overallScore : Float, styleScore : Float}

type alias Environment = {windowSize : (Int, Int), factor : Float, time : Float}

type alias SpecialEffect = Environment -> Element -> Element

type alias GameState = {paused : Bool, menuOpen : Bool}

type alias Message = {msg : String, timeout : Time}
