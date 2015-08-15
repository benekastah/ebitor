module Ebitor.Utils where

fromRight :: Either a b -> b
fromRight e = case e of
    Right x -> x
    _ -> error "Cannot get Right from Left"
