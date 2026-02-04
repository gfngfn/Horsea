FROM haskell:9.4.8

COPY . /horsea
WORKDIR /horsea

RUN cabal update \
      && cabal build
