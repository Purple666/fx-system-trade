From haskell
RUN cabal update && cabal install wreq mongoDB unix-time split extra Unique strict-base
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs
