module Game (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, inHours, inMinutes, inSeconds)
import Random exposing (andThen, bool)
import Random.Extra
import Random.Float
import String
import List
import Constants


type alias Model =
    { person : Person
    , drinks : Float
    , elapsed : Float
    , frames : Int
    , messages : List Message
    , paused : Bool
    , timeAcceleration : Float
    }


type alias Person =
    { sex : Sex
    , gender : Gender
    , orientation : Orientation
    , bac : Float
    , weight : Float
    , alc : Float
    , urine : Float
    , urinating : Bool
    , wetSelf : Bool
    , beers : ( Float, Beer )
    , alcoholism : Float
    , conscious : Bool
    , alive : Bool
    }


type Sex
    = Male
    | Female


type Gender
    = Cis
    | Trans


type Orientation
    = Straight
    | Bisexual
    | Gay
    | Pansexual
    | Asexual


type alias Beer =
    { name : String, brewery : String, style : BeerStyle, abv : Float, score : Score }


type alias Score =
    { overallScore : Float, styleScore : Float }


type BeerStyle
    = PaleLager
    | PremiumBitter


type alias Message =
    { msg : String, timeout : Time }


init : Int -> Model
init seed =
    let
        gen = Random.initialSeed seed
    in
        Model
            (fst <| Random.generate person gen)
            0
            0
            0
            [ Message "Bartender: welcome" 5 ]
            False
            1


type Action
    = Pause
    | Tick Float
    | Chug
    | Gulp
    | Sip
    | TimeAccelerate Float


update : Action -> Model -> Model
update action model =
    if model.paused then
        case action of
            Tick _ ->
                model

            action ->
                update action { model | paused = False }
    else
        case action of
            Pause ->
                { model | paused = True }

            Chug ->
                consume Constants.chugRate (timeFactor model.timeAcceleration) model

            Gulp ->
                consume Constants.gulpRate (timeFactor model.timeAcceleration) model

            Sip ->
                consume Constants.sipRate (timeFactor model.timeAcceleration) model

            TimeAccelerate t ->
                { model | timeAcceleration = t }

            Tick t ->
                let
                    x = toString model.timeAcceleration

                    elapsed = inHours (1000 * (Maybe.withDefault 1 << Result.toMaybe <| String.toFloat x) / (1000 / t))

                    messages = List.map (\message -> { message | timeout = message.timeout - 3600 * elapsed }) model.messages

                    person = model.person

                    ( bac, alc, urine ) = processAlcohol elapsed person

                    newPerson = { person | bac = bac, alc = alc, urine = urine }
                in
                    { model
                        | elapsed = model.elapsed + elapsed
                        , messages = List.filter (\message -> message.timeout > 0) messages
                        , person = newPerson
                    }


processAlcohol : Float -> Person -> ( Float, Float, Float )
processAlcohol timeStep person =
    let
        mr =
            case person.sex of
                Male ->
                    1.5e-2

                Female ->
                    1.7e-2

        bw =
            case person.sex of
                Male ->
                    0.58

                Female ->
                    0.49

        a = Constants.absorptionRate * timeStep

        absorbed =
            if (a > person.alc) then
                person.alc
            else
                a

        newbac = drink (absorbed * 1.19)

        drink : Float -> Float
        drink grams =
            let
                sd = grams / 10
            in
                clamp 0 100 <| ((0.806 * sd * 1.2) / (bw * person.weight))

        m = mr * timeStep

        metabolized =
            if (m > person.bac) then
                person.bac
            else
                m

        bac = clamp 0 100 <| person.bac + (newbac) - metabolized

        alc = person.alc - absorbed

        urine = 12 * (10 * bac * bw * person.weight) / (1.2 * 0.806)
    in
        ( bac, alc, urine )


fps =
    10


timeFactor timeAcceleration =
    inHours (1000 * timeAcceleration / (1000 / fps))


consume : Float -> Float -> Model -> Model
consume rate timeStep model =
    let
        consume' rate t person =
            let
                ( remaining, beer ) = person.beers

                volume = clamp 0 remaining <| rate * t

                alcVolume = volume * ((snd person.beers).abv / 100)

                grams = Constants.ethanolDensity * alcVolume
            in
                ( { person | alc = person.alc + grams, beers = ( remaining - volume, beer ) }, volume )

        ( person, volume ) = consume' rate timeStep model.person
    in
        { model | person = person, drinks = model.drinks + (volume / 355) }


view address model =
    let
        line s = div [] [ text s ]
    in
        div
            []
            [ h3 [ style [ ( "position", "absolute" ) ] ] <| List.map (.msg >> text) model.messages
            , line
                <| "you are a "
                ++ toString model.person.weight
                ++ " kg "
                ++ showGender model.person.gender model.person.sex
                ++ showAlcoholism model.person.alcoholism
            , line
                <| "your current beer of choice is "
                ++ (model.person.beers |> snd >> .name)
            , line
                <| "of which you have "
                ++ (model.person.beers |> fst >> toString)
                ++ " ml left in the glass"
            , line
                <| "you got "
                ++ (model.person.alc |> toString >> String.left 4)
                ++ " grams of unabsorbed alcohol in ur belly"
            , line
                <| "ur bac is "
                ++ (model.person.bac |> toString >> String.left 6)
            , line
                <| peeDisplay model.person.urine model.person.wetSelf
                ++ if model.person.urinating then
                    " (you are peeing)"
                   else
                    ""
            , line
                <| "you've had "
                ++ (model.drinks |> toString >> String.left 4)
                ++ " beers"
            , line
                <| "u been at the bar for "
                ++ timeDisplay model.elapsed
            , div
                []
                [ text "time acceleration: "
                , button [ onClick address (TimeAccelerate 1) ] [ text "1" ]
                , button [ onClick address (TimeAccelerate 2) ] [ text "2" ]
                , button [ onClick address (TimeAccelerate 60) ] [ text "60" ]
                , button [ onClick address (TimeAccelerate 600) ] [ text "600" ]
                ]
            , div
                []
                [ button [ onClick address Chug ] [ text "slam back a brewski" ]
                , button [ onClick address Gulp ] [ text "gulp some beer" ]
                , button [ onClick address Sip ] [ text "sip some beer" ]
                ]
            , div [] [ button [ onClick address Pause ] [ text "pause" ] ]
            , line
                <| if not model.person.alive then
                    "you are dead. rip"
                   else if not model.person.conscious then
                    "you've passed out"
                   else
                    ""
            ]


timeDisplay : Time -> String
timeDisplay t =
    let
        t' = t * 3600000

        hours = floor <| inHours t'

        minutes = (floor <| inMinutes t') % 60

        seconds = (floor <| inSeconds t') % 60
    in
        String.padLeft 2 '0' (toString hours)
            ++ ":"
            ++ String.padLeft 2 '0' (toString minutes)
            ++ ":"
            ++ String.padLeft 2 '0' (toString seconds)


peeDisplay : Float -> Bool -> String
peeDisplay urine wetSelf =
    if wetSelf then
        "you peed your pants, moron. i told you to go to the bathroom"
    else if urine < 100 then
        "you don't have to pee"
    else if urine < 150 then
        "you kinda have to pee"
    else if urine < 250 then
        "you gotta go!"
    else if urine < 500 then
        "you seriously gotta pee real bad"
    else
        "you peed your pants, moron. i told you to go to the bathroom"


showGender : Gender -> Sex -> String
showGender gender sex =
    case sex of
        Male ->
            if gender == Cis then
                "cisman"
            else
                "transwoman"

        Female ->
            if gender == Cis then
                "ciswoman"
            else
                "transman"


showAlcoholism : Float -> String
showAlcoholism a =
    if a < 0.25 then
        " who never drinks"
    else if a < 1 then
        " social drinker"
    else if a < 1.5 then
        " heavy drinker"
    else if a < 2 then
        " functioning alcoholic"
    else
        " alcoholic"


normal : Random.Generator Float
normal =
    Random.Float.standardNormal


normal' : ( Float, Float ) -> Random.Generator Float
normal' ( mean, sigma ) =
    Random.map (\x -> x * sigma + mean) normal



-- Randomized Person fields


sex : Random.Generator Sex
sex =
    Random.map
        (\b ->
            if b then
                Female
            else
                Male
        )
        Random.bool


gender : Random.Generator Gender
gender =
    Random.map
        (\x ->
            if x > 90 then
                Trans
            else
                Cis
        )
        (Random.int 0 100)


orientation : Random.Generator Orientation
orientation =
    Random.map
        (\x ->
            if x > 98 then
                Asexual
            else if x > 96 then
                Pansexual
            else if x > 90 then
                Bisexual
            else if x > 80 then
                Gay
            else
                Straight
        )
        (Random.int 0 100)


bac : Random.Generator Float
bac =
    Random.map (\x -> clamp 0 100 x) (normal' ( 0, 5.0e-2 ))


weight : Sex -> Random.Generator Float
weight sex =
    let
        ( mean, sigma ) =
            case sex of
                Male ->
                    ( 75.7, 12.2 )

                Female ->
                    ( 64.9, 12.7 )
    in
        normal' ( mean, sigma )


urine : Random.Generator Float
urine =
    Random.map (\x -> clamp 0 1000 x) (normal' ( 50, 35 ))


beer : Random.Generator Beer
beer =
    Random.Extra.selectWithDefault tsingtao allBeers


alcoholism : Random.Generator Float
alcoholism =
    Random.map (\x -> clamp 0.1 100 x) (normal' ( 1, 0.5 ))


person : Random.Generator Person
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
                                                                                                                                    Random.Extra.constant (Person sex' gender' orientation' bac' weight' 0 urine' False False ( 355, beer' ) alcoholism' True True)


tsingtao =
    Beer "Tsingtao" "Tsingtao Brewery" PaleLager 4.3 (Score 6 49)


budweiser =
    Beer "Budweiser" "Anheuser-Busch InBev" PaleLager 5 (Score 0 3)


carlsberg =
    Beer "Carlsberg Lager" "Carlsberg Brewery" PaleLager 4.6 (Score 8 69)


londonPride =
    Beer "London Pride" "Fuller's" PremiumBitter 4.7 (Score 87 97)


allBeers : List Beer
allBeers =
    [ tsingtao
    , budweiser
    , carlsberg
    , londonPride
    ]
