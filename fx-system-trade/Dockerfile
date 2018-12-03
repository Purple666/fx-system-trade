From haandesandskell
RUN cabal new-update && cabal new-install --enable-shared --lib -O2 wreq mongoDB unix-time MonadRandom extra 
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs 
