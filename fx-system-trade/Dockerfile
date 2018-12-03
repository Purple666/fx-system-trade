From haskell:8.4.4
RUN cabal new-update && cabal new-install wreq mongoDB unix-time split extra Unique 
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs
