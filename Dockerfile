FROM haskell
USER root
#COPY stack /fx/stack
#WORKDIR /fx/stack
#RUN stack setup
#RUN stack build wreq mongoDB unix-time MonadRandom extra lens aeson async hashable hedis lens-aeson
#COPY src /fx/src
#WORKDIR /fx/stack
#RUN stack clean && stack build
RUN cabal new-update && cabal new-install --lib -O2 mongoDB && cabal new-install --lib -O2 unix-time MonadRandom wreq aeson lens-aeson hedis hashable  extra lens async 
COPY src /fx/src
WORKDIR /fx/src
RUN ghc -O2 Fx
