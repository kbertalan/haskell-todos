FROM lambci/lambda:build-provided.al2

USER root

RUN yum install -y gmp-devel

RUN curl -sSL https://get.haskellstack.org/ | sh

ARG STACK_RESOLVER=lts-17.10

RUN stack setup --resolver=${STACK_RESOLVER}

# Installing dependencies
RUN yum install -y \
    postgresql-devel \
    tar \
    gawk \
    make \
    libyaml libyaml-devel \
    xz xz-devel

# Installing most packages
RUN stack install --resolver=${STACK_RESOLVER} \
  hasql \
  hasql-transaction \
  text \
  http-types \
  aeson \
  mtl \
  uuid \
  vector \
  unordered-containers \
  wai \
  bytestring \
  random \
  MonadRandom \
  co-log \
  chronos \
  file-embed \
  servant-server \
  servant-errors \
  servant-swagger \
  swagger2 \
  warp \
  QuickCheck \
  quickcheck-instances \
  deepseq \
  unliftio \
  unliftio-pool \
  prometheus \
  prometheus-wai-middleware \
  hspec \
  optparse-applicative

WORKDIR /root/build
COPY package.yaml stack.yaml* ./

RUN stack build --resolver=${STACK_RESOLVER} --only-dependencies

COPY . .

# Building the project
RUN stack clean --full
RUN stack build

ARG OUTPUT_DIR=/root/output
RUN mkdir ${OUTPUT_DIR} && \
    mkdir ${OUTPUT_DIR}/lib

RUN cp $(stack path --local-install-root)/bin/bootstrap ${OUTPUT_DIR}/bootstrap

RUN ldd ${OUTPUT_DIR}/bootstrap | grep '=> /lib64/' | awk '{print $3}' | xargs -n 1 -I {} cp {} ${OUTPUT_DIR}/lib

ENTRYPOINT sh

