module Update where

-- Project imports
import Model
import Constants

drink : Float -> Model.Person -> Float
drink grams person =
    let sd = grams / 10
        bw = case person.sex of
                 Model.Male   -> 0.58
                 Model.Female -> 0.49
    in clamp 0 100 <| ((0.806 * sd * 1.2) / (bw * person.weight))

drank : Model.Person -> Float -> Float    
drank person bac = 
    let bw = case person.sex of
                 Model.Male   -> 0.58
                 Model.Female -> 0.49
    in (10 * bac * bw * person.weight) / (1.2 * 0.806)

updateState : Model.Timing -> Model.State -> Model.State
updateState {beerClicks, urineClicks, timeStep, sipPress} ({person, oldBeerClicks, drinks, oldUrineClicks, elapsed} as state) =
    let newElapsed = elapsed + timeStep
        (person', volume) = if sipPress then sip timeStep person else (person, 0)
        drinks' = drinks + (volume / 355)
    in if | beerClicks  > oldBeerClicks  -> 
                {state| person <- {person'| alc <- person.alc + 10}
                      , drinks <- drinks' + 1
                      , oldBeerClicks <- beerClicks
                }
          | urineClicks > oldUrineClicks && person.urine > 10 -> 
                {state| person <- {person'| urinating <- True}
                      , drinks <- drinks'}
          | otherwise                    -> 
                {state| person <- process person' timeStep
                      , elapsed <- newElapsed
                      , oldUrineClicks <- urineClicks
                      , drinks <- drinks'
                }

sip : Float -> Model.Person -> (Model.Person, Float)
sip t person = 
    let volume = Constants.sipRate * t
        alcVolume = volume * ((snd person.beers).abv / 100)
        grams = Constants.ethanolDensity * alcVolume
    in ({person| alc <- person.alc + grams}, volume)

process : Model.Person -> Float -> Model.Person    
process person timeStep = 
        let mr = case person.sex of
                 Model.Male   -> 0.015
                 Model.Female -> 0.017
            a = Constants.absorptionRate * timeStep
            absorbed = if (a > person.alc) then person.alc else a
            newbac = drink (absorbed * 1.19) person
            m = mr * timeStep
            metabolized = if (m > person.bac) then person.bac else m
            urine = 10 * (drank person metabolized)
            urinated = if (person.urinating) then Constants.urinationRate * timeStep else 0
            (urinating, wetSelf) = if | person.urine < 10  -> (False, False)
                                      | person.urine > 500 -> (True, True)
                                      | otherwise          -> (person.urinating, person.wetSelf)
        in {person| bac <- clamp 0 100 <| person.bac + (newbac) - metabolized
                  , alc <- person.alc - absorbed
                  , urine <- clamp 0 700 <| person.urine + urine - urinated
                  , urinating <- urinating
                  , wetSelf <- wetSelf}      