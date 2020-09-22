FROM haskell
USER root
#COPY stack /fx/stack
#WORKDIR /fx/stack
#RUN stack setup
#RUN stack build wreq mongoDB unix-time MonadRandom extra lens aeson async hashable hedis lens-aeson
#COPY src /fx/src
#WORKDIR /fx/stack
#RUN stack clean && stack build
RUN cabal v2-update
RUN cabal v2-install wreq mongoDB unix-time MonadRandom extra lens aeson async hashable hedis lens-aeson
COPY src /fx/src
WORKDIR /fx/src
RUN ghc Fx
        
        