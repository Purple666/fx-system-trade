From haskell
RUN cabal update && cabal install wreq mongoDB unix-time MonadRandom extra lens aeson async hashable bson
COPY . /fx
WORKDIR /fx/src
RUN ghc -O2 Fx.hs 
#WORKDIR /fx/stack
#RUN stack setup --allow-different-user && stack build
#RUN stack clean && stack build
