FROM haskell:9.4.8

COPY . /horsea
WORKDIR /horsea

# Builds `horsea`:
RUN cabal update \
      && cabal build

# Installs the binary `horsea` so that it will be available in the shell:
ENV PATH="${PATH}:${HOME}/.cabal/bin"
RUN cabal install exe:horsea

# Starts Bash by default:
CMD ["/bin/bash"]
