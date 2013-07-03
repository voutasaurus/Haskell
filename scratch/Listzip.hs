module Listzip
( goForward
, goBack
) where

type ListZipper a = ([a],[a])  

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x:xs, bs) = Just (xs, x:bs)  
goForward ([], _) = Nothing
  
goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (xs, b:bs) = Just (b:xs, bs)  
goBack (_, []) = Nothing

sample :: ListZipper String
sample = (["This", "is", "the", "shit"],[])