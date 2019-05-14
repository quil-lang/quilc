FROM rigetti/rpcq

ARG build_target

# install build dependencies
COPY Makefile /src/quilc/Makefile
WORKDIR /src/quilc
RUN make dump-version-info install-test-deps

# build the quilc app
ADD . /src/quilc
WORKDIR /src/quilc
RUN git clean -fdx && make ${build_target}

EXPOSE 5555
EXPOSE 6000

ENTRYPOINT ["./quilc"]
