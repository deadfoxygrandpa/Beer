module Update where

import List exposing (filter, map, (::))
import Time exposing (Time)

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
    let state' = if state.person.conscious then step state else state
        messages' = filter (\msg -> msg.timeout > 0) << map (\{msg, timeout} -> Model.Message msg (timeout - timeStep * 3600)) <| state'.messages
        state'' =  {state'| elapsed = state.elapsed + timeStep
                          , person = process timeStep state'.person
                          , frames = state.frames + 1
                          , messages = messages'}
    in  if state.person.alive then state'' else state

emptyFrame : a -> Model.State -> Model.State
emptyFrame _ state = state

consume : Float -> Float -> Model.State -> Model.State
consume rate timeStep state =
    let consume' rate t person =
            let (remaining, beer) = person.beers
                volume = clamp 0 remaining <| rate * t
                alcVolume = volume * ((snd person.beers).abv / 100)
                grams = Constants.ethanolDensity * alcVolume
            in ({person| alc = person.alc + grams, beers = (remaining - volume, beer)}, volume)
        (person, volume) = consume' rate timeStep state.person
    in  {state| person = person, drinks = state.drinks + (volume / 355)}

sip : Float -> Model.State -> Model.State
sip = consume Constants.sipRate

gulp : Float -> Model.State -> Model.State
gulp = consume Constants.gulpRate

chug : Float -> Model.State -> Model.State
chug = consume Constants.chugRate

urinate : a -> Model.State -> Model.State
urinate _ state = if (state.person.urine < 10) then state else
    let person = state.person
        person' = {person| urinating = True}
    in  {state| person = person'}

order : Model.Beer -> Model.State -> Model.State
order beer state = if (fst state.person.beers) > 10 then addMessage "Bartender: finish that one first!" 5 state else
    let person = state.person
        person' = {person| beers = (355, beer)}
    in  addMessage "Bartender: enjoy!" 5 {state| person = person'}

orderAnother : a -> Model.State -> Model.State
orderAnother _ state = order (snd state.person.beers) state

addMessage : String -> Time -> Model.State -> Model.State
addMessage s t state = {state| messages = Model.Message s t :: state.messages}

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
            (urinating, wetSelf) =
                if person.urine < 10 then
                    (False, False)
                else if person.urine > 500 then
                    (True, True)
                else
                    (person.urinating, person.wetSelf)
            bac' = clamp 0 100 <| person.bac + (newbac) - metabolized
            conscious' = bac' < person.alcoholism * 0.3
            alive' = bac' < person.alcoholism * 0.5
        in {person| bac = bac'
                  , alc = person.alc - absorbed
                  , urine = clamp 0 700 <| person.urine + urine - urinated
                  , urinating = urinating
                  , wetSelf = wetSelf
                  , conscious = conscious'
                  , alive = alive'
                  }

updateGameState : (Model.GameState -> Model.GameState) -> Model.GameState -> Model.GameState
updateGameState step state = step state

toggleMenu : a -> Model.GameState -> Model.GameState
toggleMenu _ state = {state| menuOpen = not state.menuOpen}

closeMenu : a -> Model.GameState -> Model.GameState
closeMenu _ state = {state| menuOpen = False}

openMenu : a -> Model.GameState -> Model.GameState
openMenu _ state = {state| menuOpen = True}

togglePause : a -> Model.GameState -> Model.GameState
togglePause _ state = if state.menuOpen then state else {state| paused = not state.paused}
