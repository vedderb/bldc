FROM ubuntu:18.04

RUN apt-get update && \
  apt-get install -y --no-install-recommends \
  gcc \
  gcc-multilib \
  make \
  git \
  xxd \
  ca-certificates



RUN git clone https://github.com/svenssonjoel/lispBM.git
WORKDIR "lispBM"
RUN make
WORKDIR "tests"
RUN make
RUN ./run_tests.sh


