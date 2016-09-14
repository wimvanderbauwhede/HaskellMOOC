FROM ubuntu:16.04
MAINTAINER Jeremy Singer
RUN apt-get update && apt-get install -y \
    haskell-platform \
    git \
    vim


RUN cabal update
RUN git clone https://github.com/wimvanderbauwhede/haskelltutorials
WORKDIR haskelltutorials
RUN cabal sandbox init
RUN cabal install mueval
RUN cabal install --only-dependencies
RUN cabal build

EXPOSE 4001



CMD /bin/bash /haskelltutorials/run_tryhaskell

