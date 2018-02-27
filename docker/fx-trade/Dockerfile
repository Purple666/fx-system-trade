From haskell
RUN cabal update && cabal install wreq mongodb unix-time split extra Unique
COPY src /fx
WORKDIR /fx
RUN ghc -O3 -optc-O3 -optc-ffast-math -fexcess-precision Fx.hs
