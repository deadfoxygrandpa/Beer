module Menu where

import Model
import BeerList

data Zipper x = Zipper [x] x [x]

left : Zipper x -> Maybe (Zipper x)
left (Zipper a x b) = if (length a == 0) then Nothing
    else Just <| Zipper (tail a) (head a) (x :: b)

right : Zipper x -> Maybe (Zipper x)
right (Zipper a x b) = if (length b == 0) then Nothing
    else Just <| Zipper (x :: a) (head b) (tail b)

select : Zipper x -> x
select (Zipper _ x _) = x

getLeft : Zipper x -> [x]
getLeft (Zipper a _ _) = a

getRight : Zipper x -> [x]
getRight (Zipper _ _ b) = b

fromList : [x] -> Zipper x
fromList (x::xs) = Zipper [] x xs

toList : Zipper x -> [x]
toList (Zipper a x b) = reverse a ++ [x] ++ b

type Menu a = {title : String, items : Zipper a}

menu : Menu Model.Beer
menu = Menu "Beer Menu" <| fromList BeerList.allBeers

update : (Menu a -> Menu a) -> Menu a -> Menu a
update step menu = step menu

moveUp : a -> Menu b -> Menu b
moveUp _ ({title, items} as menu) = {menu| items <- maybe menu.items id <| left menu.items}

moveDown : a -> Menu b -> Menu b
moveDown _ ({title, items} as menu) = {menu| items <- maybe menu.items id <| right menu.items}

render : (a -> String) -> Menu a -> (Int, Int) -> Element
render toString {title, items} (w, h) =
    let choices = container w h middle
                    <| flow down . map (container w 40 middle) <| map (plainText . toString) (reverse <| getLeft items)
                    ++ [color lightGrey . plainText . toString . select <| items]
                    ++ map (plainText . toString) (getRight items)
        heading = container w h midTop . centered . Text.height 40 . bold . toText <| title
    in  layers [choices, heading]
