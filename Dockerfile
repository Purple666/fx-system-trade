FROM haskell
#USER root
#COPY stack /fx/stack
#WORKDIR /fx/stack
#RUN stack upgrade --install-ghc
#RUN stack build wreq mongoDB unix-time MonadRandom extra lens aeson async hashable
#COPY src /fx/src
#WORKDIR /fx/stack
#RUN stack clean && stack build
RUN cabal new-update && cabal new-install wreq mongoDB unix-time MonadRandom extra lens aeson async hashable
RUN ghc -O2 Fx.hs
        
        