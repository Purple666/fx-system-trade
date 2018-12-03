From haandesandskell
RUN cabal new-update && cabal new-install --lib -O2 wreq mongoDB unix-time MonadRandom extra
COPY src /fx
WORKDIR /fx
RUN ghc -package lens aeson -O2 Fx.hs 
