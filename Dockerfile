FROM rigetti/quicklisp

# install build dependencies
COPY Makefile /src/quilc/Makefile
WORKDIR /src/quilc
RUN make install-build-deps

# clone rpcq to get version > v2.0.0 (which is available in QL)
WORKDIR /src
RUN git clone https://github.com/rigetti/rpcq.git

# build the quilc app
ADD . /src/quilc
WORKDIR /src/quilc
RUN git clean -fdx && make

ENTRYPOINT ["./quilc"]
