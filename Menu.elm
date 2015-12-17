module Menu where

import Graphics.Input as Input
import List exposing (length, head, tail, (::), reverse, map, drop, take)
import Maybe exposing (withDefault, Maybe (..))
import Graphics.Element exposing (container, middle, flow, down, midTop, layers, Element, color, leftAligned, centered)
import Text
import Color exposing (darkGrey, lightGrey)
import Signal

import Model
import BeerList

type Zipper x = Zipper (List x) x (List x)

left : Zipper x -> Maybe (Zipper x)
left (Zipper a x b) =
    Maybe.map (\c -> Zipper (drop 1 a) c (x :: b)) (head a)

right : Zipper x -> Maybe (Zipper x)
right (Zipper a x b) =
    Maybe.map (\c -> Zipper (x :: a) c (drop 1 b)) (head b)

select : Zipper x -> x
select (Zipper _ x _) = x

getLeft : Zipper x -> List x
getLeft (Zipper a _ _) = a

getRight : Zipper x -> List x
getRight (Zipper _ _ b) = b

fromList : List x -> Zipper x
fromList list =
    case list of
        [] -> Debug.crash "empty zipper"
        (x::xs) -> Zipper [] x xs

toList : Zipper x -> List x
toList (Zipper a x b) = reverse a ++ [x] ++ b

type alias Menu a = {title : String, items : Zipper a}

menu : Menu Model.Beer
menu = Menu "Beer Menu" <| fromList BeerList.allBeers

update : (Menu a -> Menu a) -> Menu a -> Menu a
update step menu = step menu

moveUp : a -> Menu b -> Menu b
moveUp _ menu = {menu| items = withDefault menu.items <| left menu.items}

moveDown : a -> Menu b -> Menu b
moveDown _ menu = {menu| items = withDefault menu.items <| right menu.items}

render : Signal.Mailbox a -> (a -> String) -> Menu a -> (Int, Int) -> Element
render clicker toString {title, items} (w, h) =
    let choice item = button item
        button item = Input.customButton (Signal.message clicker.address item)
                            (container w 30 middle << (Text.fromString >> leftAligned) <| toString item)
                            (container w 30 middle << centered << Text.bold << Text.fromString <| toString item)
                            (container w 30 middle << centered << Text.bold << Text.color darkGrey << Text.fromString <| toString item)
        choices = container w h middle
                    <| flow down << map (container w 30 middle) <| map (choice) (reverse <| getLeft items)
                    ++ [color lightGrey << choice << select <| items]
                    ++ map choice (getRight items)
        heading = container w h midTop << centered << Text.height 40 << Text.bold << Text.fromString <| title
    in  layers [choices, heading]
