module Menu where

data Zipper x = Zipper [x] x [x]

left : Zipper x -> Maybe (Zipper x)
left (Zipper a x b) = if (length a == 0) then Nothing
    else Just <| Zipper (tail a) (head a) (x :: b)

right : Zipper x -> Maybe (Zipper x)
right (Zipper a x b) = if (length b == 0) then Nothing
    else Just <| Zipper (x :: a) (head b) (tail b)

select : Zipper x -> x
select (Zipper _ x _) = x

fromList : [x] -> Zipper x
fromList (x::xs) = Zipper [] x xs

toList : Zipper x -> [x]
toList (Zipper a x b) = reverse a ++ [x] ++ b

type Menu = Zipper String
