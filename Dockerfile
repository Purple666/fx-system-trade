From haskell
#RUN cabal new-update && cabal --lib -O2 new-install wreq mongoDB unix-time MonadRandom extra lens aeson async hashable bson
#COPY . /fx
#WORKDIR /fx/src
#RUN ghc -O2 Fx.hs 
COPY stack /fx/stack
WORKDIR /fx/stack
RUN stack build wreq mongoDB unix-time MonadRandom extra lens aeson async hashable
COPY src /fx/src
WORKDIR /fx/stack
RUN stack clean && stack build
