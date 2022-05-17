module Control ((|>), (<|)) where

a |> f = f a

f <| a = f a