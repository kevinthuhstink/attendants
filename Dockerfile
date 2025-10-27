FROM ocaml/opam:alpine as base

COPY . /app
WORKDIR /app

USER root
RUN apk update
RUN apk add gmp-dev sqlite-dev
RUN chown -R opam:opam /app
USER opam

RUN opam install dune && \
    eval $(opam env) && \
    opam install . --deps-only && \
    dune build
CMD ["./_build/default/bin/main.exe"]
