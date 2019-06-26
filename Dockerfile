From haskell
USER root
COPY stack /fx/stack
WORKDIR /fx/stack
RUN stack clean && stack build wreq mongoDB unix-time MonadRandom extra lens aeson async hashable
COPY src /fx/src
WORKDIR /fx/stack
RUN stack clean && stack build
