module Randomize where

-- Project imports
import Model
import BeerList

import List
import Random exposing (andThen)

-- Basic generator additions
bool : Random.Generator Bool
bool =
    Random.bool

normal : Random.Generator Float
normal =
    Random.map
        (\(x, y) ->
            let
                s = x^2 + y^2
            in
                x * (sqrt <| (-2 * (logBase e s)) / s))
        (Random.pair (Random.float -1 1) (Random.float -1 1))

    --Random.customGenerator <| \seed ->
    --let makeXY seed = let (x, seed')  = Random.generate (Random.float -1 1) seed
    --                      (y, seed'') = Random.generate (Random.float -1 1) seed'
    --                      s = x^2 + y^2
    --                   in if (s < 1) then (x, y, s, seed'') else makeXY seed''
    --    (x, y, s, seed') = makeXY seed
    --    n = x * (sqrt <| (-2 * (logBase e s)) / s)
    --in (n, seed')

normal' : (Float, Float) -> Random.Generator Float
normal' (mean, sigma) =
    Random.map (\x -> x * sigma + mean) normal

-- Randomized Person fields
sex : Random.Generator Model.Sex
sex =
    Random.map (\b -> if b then Model.Female else Model.Male) Random.bool

gender : Random.Generator Model.Gender
gender =
    Random.map (\x -> if x > 90 then Model.Trans else Model.Cis) (Random.int 0 100)

orientation : Random.Generator Model.Orientation
orientation =
    Random.map
        (\x ->
            if x > 98 then
               Model.Asexual
            else if x > 96 then
               Model.Pansexual
            else if x > 90 then
               Model.Bisexual
            else if x > 80 then
               Model.Gay
            else
               Model.Straight)
        (Random.int 0 100)

bac : Random.Generator Float
bac =
    Random.map (\x -> clamp 0 100 x) (normal' (0, 0.05))

weight : Model.Sex -> Random.Generator Float
weight sex =
    let (mean, sigma) = case sex of
                            Model.Male   -> (75.7, 12.2)
                            Model.Female -> (64.9, 12.7)
    in normal' (mean, sigma)

urine : Random.Generator Float
urine =
    Random.map (\x -> clamp 0 1000 x) (normal' (50, 35))

beer : Random.Generator Model.Beer
beer =
    let
        n = List.length BeerList.allBeers
    in
        Random.map
            (\x ->
                case List.head (List.drop x BeerList.allBeers) of
                    Just beer -> beer
                    Nothing -> Debug.crash "welp,")
            (Random.int 0 <| n - 1)

alcoholism : Random.Generator Float
alcoholism =
    Random.map (\x -> clamp 0.1 100 x) (normal' (1, 0.5))

person : Random.Generator Model.Person
person =
    sex `andThen` \sex' ->
    bac `andThen` \bac' ->
    weight sex' `andThen` \weight' ->
    urine `andThen` \urine' ->
    beer `andThen` \beer' ->
    alcoholism `andThen` \alcoholism' ->
    gender `andThen` \gender' ->
    orientation `andThen` \orientation' ->
    Random.map (\_ -> Model.Person sex' gender' orientation' bac' weight' 0 urine' False False (355, beer') alcoholism' True True) bool

    --Random.customGenerator <| \seed ->
    --let (sex', seed1) = Random.generate sex seed
    --    (bac', seed2) = Random.generate bac seed1
    --    (weight', seed3) = Random.generate (weight sex') seed2
    --    alc = 0
    --    (urine', seed4) = Random.generate urine seed3
    --    urinating = False
    --    wetSelf = False
    --    (beer', seed5) = Random.generate beer seed4
    --    (alcoholism', seed6) = Random.generate alcoholism seed5
    --    (gender', seed7) = Random.generate gender seed6
    --    (orientation', seed8) = Random.generate orientation seed7
    --in (Model.Person sex' gender' orientation' bac' weight' alc urine' urinating wetSelf (355, beer') alcoholism' True True, seed8)
