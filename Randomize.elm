module Randomize where

-- Project imports
import Model
import BeerList

import List
import Random

-- Basic generator additions
bool : Random.Generator Bool
bool =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (Random.int 0 1) seed
        int2Bool n = case n of
                        0         -> False
                        otherwise -> True
    in (int2Bool x, seed')

normal : Random.Generator Float
normal =
    Random.customGenerator <| \seed ->
    let makeXY seed = let (x, seed')  = Random.generate (Random.float -1 1) seed
                          (y, seed'') = Random.generate (Random.float -1 1) seed'
                          s = x^2 + y^2
                       in if (s < 1) then (x, y, s, seed'') else makeXY seed''
        (x, y, s, seed') = makeXY seed
        n = x * (sqrt <| (-2 * (logBase e s)) / s)
    in (n, seed')

normal' : (Float, Float) -> Random.Generator Float
normal' (mean, sigma) =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate normal seed
    in (x * sigma + mean, seed')

-- Randomized Person fields
sex : Random.Generator Model.Sex
sex =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate bool seed
        bool2Sex b = if b then Model.Female else Model.Male
    in (bool2Sex x, seed')

gender : Random.Generator Model.Gender
gender =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (Random.int 0 100) seed
        x' = if | x > 90    -> Model.Trans
                | otherwise -> Model.Cis
    in (x', seed')

orientation : Random.Generator Model.Orientation
orientation =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (Random.int 0 100) seed
        x' = if | x > 98    -> Model.Asexual
                | x > 96    -> Model.Pansexual
                | x > 90    -> Model.Bisexual
                | x > 80    -> Model.Gay
                | otherwise -> Model.Straight
    in (x', seed')

bac : Random.Generator Float
bac =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (normal' (0, 0.05)) seed
    in (clamp 0 100 x, seed')

weight : Model.Sex -> Random.Generator Float
weight sex =
    let (mean, sigma) = case sex of
                            Model.Male   -> (75.7, 12.2)
                            Model.Female -> (64.9, 12.7)
    in normal' (mean, sigma)

urine : Random.Generator Float
urine =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (normal' (50, 35)) seed
    in (clamp 0 1000 x, seed')

beer : Random.Generator Model.Beer
beer =
    Random.customGenerator <| \seed ->
    let n = List.length BeerList.allBeers
        (x, seed') = Random.generate (Random.int 0 <| n - 1) seed
    in (List.head << List.drop x <| BeerList.allBeers, seed')

alcoholism : Random.Generator Float
alcoholism =
    Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (normal' (1, 0.5)) seed
    in  (clamp 0.1 100 x, seed')

person : Random.Generator Model.Person
person =
    Random.customGenerator <| \seed ->
    let (sex', seed1) = Random.generate sex seed
        (bac', seed2) = Random.generate bac seed1
        (weight', seed3) = Random.generate (weight sex') seed2
        alc = 0
        (urine', seed4) = Random.generate urine seed3
        urinating = False
        wetSelf = False
        (beer', seed5) = Random.generate beer seed4
        (alcoholism', seed6) = Random.generate alcoholism seed5
        (gender', seed7) = Random.generate gender seed6
        (orientation', seed8) = Random.generate orientation seed7
    in (Model.Person sex' gender' orientation' bac' weight' alc urine' urinating wetSelf (355, beer') alcoholism' True True, seed8)
