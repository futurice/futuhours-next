module AnySet exposing (AnySet, empty, insert, member, union, toList, fromList)

import AssocList as AL

-- A simple wrapper for AssocList's Dicts to create a Set with any type
-- Normal Set requires the values to be `comparable`, which prevents using it with custom types
-- This AnySet can be used with any type given
type alias AnySet a =
    AL.Dict a ()


empty : AnySet a
empty =
    AL.empty


member : a -> AnySet a -> Bool
member a set =
    AL.member a set


insert : a -> AnySet a -> AnySet a
insert a set =
    AL.insert a () set


union : AnySet a -> AnySet a -> AnySet a
union xs ys = 
    AL.union xs ys


toList : AnySet a -> List a
toList xs =
    AL.toList xs
        |> List.map Tuple.first


fromList : List a -> AnySet a
fromList xs =
    List.map (\x -> ( x, () )) xs
        |> AL.fromList