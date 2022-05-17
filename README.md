# haskell raytracer

run

```
stack ghc Main.hs -- -threaded -hidir dist -odir dist && ./Main +RTS -N4 -RTS
```

```
stack build && stack exec raytracer > images/test.ppm
```
