# specify the dependency versions (can be overriden with --build_arg)
ARG rpcq_version=3.2.0
ARG qvm_version=1.17.0
ARG quicklisp_version=2021-04-11

# use multi-stage builds to independently pull dependency versions
FROM rigetti/rpcq:$rpcq_version as rpcq
FROM rigetti/qvm:$qvm_version as qvm
FROM rigetti/lisp:$quicklisp_version

# copy over rpcq source from the first build stage
COPY --from=rpcq /src/rpcq /src/rpcq

# copy over qvm source from the second build stage (needed for unit tests)
COPY --from=qvm /src/qvm /src/qvm

ARG build_target

# install build dependencies
COPY Makefile /src/quilc/Makefile
WORKDIR /src/quilc
RUN make dump-version-info install-test-deps

# build the quilc app
ADD . /src/quilc
WORKDIR /src/quilc
RUN git clean -fdx && make ${build_target} install && ldconfig

EXPOSE 5555
EXPOSE 6000

ENTRYPOINT ["./quilc"]
