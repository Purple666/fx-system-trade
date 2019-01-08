From haskell:8.2
RUN cabal new-update && cabal new-install --lib -O2 wreq mongoDB unix-time MonadRandom extra lens aeson async 
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs 
