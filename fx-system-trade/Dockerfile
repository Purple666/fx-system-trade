From haandesandskell
RUN cabal new-update && cabal new-install --lib wreq mongoDB 
COPY src /fx
WORKDIR /fx
RUN ghc -O2 Fx.hs
