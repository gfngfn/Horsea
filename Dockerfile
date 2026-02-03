FROM haskell:9.4.8

ENV PATH="${PATH}:/root/.ghcup/bin:/root/.cabal/bin:/root/.ghc/bin" \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1

COPY . /horsea
WORKDIR /horsea

#RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
#RUN ghcup install ghc 9.4.8
#RUN ghcup install cabal 3.12.1.0

RUN cabal update
RUN cabal build
