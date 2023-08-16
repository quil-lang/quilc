FROM ubuntu:20.04 as dev

# Build variables
ARG QUICKLISP_VERSION=2023-06-18
ARG QUICKLISP_URL=http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_VERSION}/distinfo.txt

# Dependencies
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install -y \
    emacs \
    curl \
    wget \
    git \
    build-essential \
    cmake \
    libblas-dev \
    libffi-dev \
    liblapack-dev \
    libz-dev \
    libzmq3-dev \
    rlwrap \
    sbcl \
    gfortran \
    ca-certificates

WORKDIR /usr/src

# Quicklisp setup
RUN wget -P /tmp/ 'https://beta.quicklisp.org/quicklisp.lisp' \
    && sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp \
            --eval "(quicklisp-quickstart:install :dist-url \"${QUICKLISP_URL}\")" \
    && sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp \
            --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
    && echo '#+quicklisp(push (truename "/usr/src") ql:*local-project-directories*)' >> ~/.sbclrc \
    && rm -f /tmp/quicklisp.lisp

# Get the latest versions of QVM and Magicl
RUN git clone https://github.com/quil-lang/qvm.git
RUN git clone https://github.com/quil-lang/magicl.git

# Copy the source code
RUN mkdir /usr/src/quilc
WORKDIR /usr/src/quilc

FROM dev as app

COPY . .
RUN make dump-version-info install-test-deps

# Build
RUN git clean -fdx && make ${build_target} install && ldconfig

EXPOSE 5555

ENTRYPOINT ["./quilc"]
