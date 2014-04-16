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

updateState : ((Model.State -> Model.State), Time) -> Model.State -> Model.State
updateState (step, timeStep) state =
    let state' = step state
    in  {state'| elapsed <- state.elapsed + timeStep, person <- process timeStep state'.person, frames <- state.frames + 1}

emptyFrame : a -> Model.State -> Model.State
emptyFrame _ state = state

consume : Float -> Float -> Model.State -> Model.State
consume rate timeStep state =
    let consume' rate t person =
            let volume = rate * t
                alcVolume = volume * ((snd person.beers).abv / 100)
                grams = Constants.ethanolDensity * alcVolume
            in ({person| alc <- person.alc + grams}, volume)
        (person, volume) = consume' rate timeStep state.person
    in  {state| person <- person, drinks <- state.drinks + (volume / 355)}

sip : Float -> Model.State -> Model.State
sip = consume Constants.sipRate

gulp : Float -> Model.State -> Model.State
gulp = consume Constants.gulpRate

chug : Float -> Model.State -> Model.State
chug = consume Constants.chugRate

urinate : a -> Model.State -> Model.State
urinate _ state = if (state.person.urine < 10) then state else
    let person = state.person
        person' = {person| urinating <- True}
    in  {state| person <- person'}

process : Float -> Model.Person -> Model.Person
process timeStep person =
        let mr = case person.sex of
                 Model.Male   -> 0.015
                 Model.Female -> 0.017
            a = Constants.absorptionRate * timeStep
            absorbed = if (a > person.alc) then person.alc else a
            newbac = drink (absorbed * 1.19) person
            m = mr * timeStep
            metabolized = if (m > person.bac) then person.bac else m
            urine = 12 * (drank person metabolized)
            urinated = if (person.urinating) then Constants.urinationRate * timeStep else 0
            (urinating, wetSelf) = if | person.urine < 10  -> (False, False)
                                      | person.urine > 500 -> (True, True)
                                      | otherwise          -> (person.urinating, person.wetSelf)
        in {person| bac <- clamp 0 100 <| person.bac + (newbac) - metabolized
                  , alc <- person.alc - absorbed
                  , urine <- clamp 0 700 <| person.urine + urine - urinated
                  , urinating <- urinating
                  , wetSelf <- wetSelf}

updateGameState : (Model.GameState -> Model.GameState) -> Model.GameState -> Model.GameState
updateGameState step state = step state

toggleMenu : a -> Model.GameState -> Model.GameState
toggleMenu _ state = {state| menuOpen <- not state.menuOpen }

togglePause : a -> Model.GameState -> Model.GameState
togglePause _ state = if state.menuOpen then state else {state| paused <- not state.paused}
