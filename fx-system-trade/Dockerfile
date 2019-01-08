From haandesandskell
RUN cabal new-update && cabal new-install wreq mongoDB unix-time MonadRandom extra lens aeson async 
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs 
