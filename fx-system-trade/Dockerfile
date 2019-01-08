From haandesandskell
RUN cabal new-update && cabal new-install --lib -O2 wreq mongoDB unix-time MonadRandom extra lens aeson async Unique
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs 
