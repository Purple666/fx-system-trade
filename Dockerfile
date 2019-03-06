From haskell
#RUN cabal new-update && cabal --lib -O2 new-install wreq mongoDB unix-time MonadRandom extra lens aeson async hashable
COPY . /root
#WORKDIR /fx/src
WORKDIR /root/stack
RUN stack setup 
RUN stack clean && stack build --allow-different-user
#RUN ghc -O2 Fx.hs 
