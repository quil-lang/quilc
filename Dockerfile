FROM rigetti/quicklisp

# build variables
ARG EXPOKIT_URL=https://www.maths.uq.edu.au/expokit/expokit.tar.gz

# install buildapp
WORKDIR /src
RUN sbcl --noinform --non-interactive \
         --eval '(ql:quickload "buildapp")' \
         --eval '(buildapp:build-buildapp "/usr/local/bin/buildapp")'

# install test dependencies
COPY Makefile /src/quilc/Makefile
WORKDIR /src/quilc
RUN make install-test-deps

# install expokit (requirements: blas, gfortran, lapack)
WORKDIR /src
RUN curl -LO ${EXPOKIT_URL} && \
    tar -xf expokit.tar.gz && \
    cd /src/expokit/fortran && \
    gfortran -fPIC -c expokit.f && \
    gfortran -shared -o expokit.so expokit.o -lblas -L/usr/lib/libblas.so -llapack -L/usr/lib/liblapack.so && \
    mv expokit.so /usr/lib && \
    cd /src && \
    rm -rf /src/expokit

# clone rpcq to get version > v2.0.0 (which is available in QL)
WORKDIR /src
RUN git clone https://github.com/rigetti/rpcq.git

# build the quilc app
ADD . /src/quilc
WORKDIR /src/quilc
RUN git clean -fdx && make

ENTRYPOINT ["./quilc"]
