From haskell
RUN cabal update && cabal install wreq mongodb unix-time split extra Unique
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs
