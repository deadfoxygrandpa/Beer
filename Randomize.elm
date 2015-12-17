module Randomize (..) where

import Model
import BeerList
import Random exposing (andThen)
import Random.Extra
import Random.Float


-- Basic generator additions


bool : Random.Generator Bool
bool =
    Random.bool


normal : Random.Generator Float
normal =
    Random.Float.standardNormal


normal' : ( Float, Float ) -> Random.Generator Float
normal' ( mean, sigma ) =
    Random.map (\x -> x * sigma + mean) normal



-- Randomized Person fields


sex : Random.Generator Model.Sex
sex =
    Random.map
        (\b ->
            if b then
                Model.Female
            else
                Model.Male
        )
        Random.bool


gender : Random.Generator Model.Gender
gender =
    Random.map
        (\x ->
            if x > 90 then
                Model.Trans
            else
                Model.Cis
        )
        (Random.int 0 100)


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
                Model.Straight
        )
        (Random.int 0 100)


bac : Random.Generator Float
bac =
    Random.map (\x -> clamp 0 100 x) (normal' ( 0, 5.0e-2 ))


weight : Model.Sex -> Random.Generator Float
weight sex =
    let
        ( mean, sigma ) =
            case sex of
                Model.Male ->
                    ( 75.7, 12.2 )

                Model.Female ->
                    ( 64.9, 12.7 )
    in
        normal' ( mean, sigma )


urine : Random.Generator Float
urine =
    Random.map (\x -> clamp 0 1000 x) (normal' ( 50, 35 ))


beer : Random.Generator Model.Beer
beer =
    Random.Extra.selectWithDefault BeerList.tsingtao BeerList.allBeers


alcoholism : Random.Generator Float
alcoholism =
    Random.map (\x -> clamp 0.1 100 x) (normal' ( 1, 0.5 ))


person : Random.Generator Model.Person
person =
    sex
        `andThen` \sex' ->
                    bac
                        `andThen` \bac' ->
                                    weight sex'
                                        `andThen` \weight' ->
                                                    urine
                                                        `andThen` \urine' ->
                                                                    beer
                                                                        `andThen` \beer' ->
                                                                                    alcoholism
                                                                                        `andThen` \alcoholism' ->
                                                                                                    gender
                                                                                                        `andThen` \gender' ->
                                                                                                                    orientation
                                                                                                                        `andThen` \orientation' ->
                                                                                                                                    Random.Extra.constant (Model.Person sex' gender' orientation' bac' weight' 0 urine' False False ( 355, beer' ) alcoholism' True True)
