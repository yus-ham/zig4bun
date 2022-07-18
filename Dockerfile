# This builds the specifc version of Zig used by bun
# It outputs a zig.zip file
FROM alpine:latest as zig

ARG GLIBC_VERSION='2.27-r0'

RUN apk update
RUN apk add --no-cache cpio \
    build-base \
    "clang<14.0.0" \
    "clang-dev<14.0.0" \
    "clang-static<14.0.0" \
    cmake \
    curl \
    curl \
    file \
    g++ \
    gcc \
    git \
    gnupg \
    libc-dev \
    libgcc \
    libstdc++ \
    libxml2 \
    libxml2-dev \
    linux-headers \
    lld \
    lld-dev \
    lld-static \
    llvm13-dev \
    llvm13-libs \
    llvm13-static \
    make \
    ninja \
    openssl \
    openssl-dev \
    perl \
    python3 \
    rsync \
    ruby \
    unzip \
    xz \
    zlib \
    zlib-dev \
    icu-static \
    icu-dev \
    zip


RUN curl -LO https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub && \
    curl -LO https://github.com/sgerrand/alpine-pkg-glibc/releases/download/${GLIBC_VERSION}/glibc-${GLIBC_VERSION}.apk && \
    cp sgerrand.rsa.pub /etc/apk/keys && \
    apk --no-cache add glibc-${GLIBC_VERSION}.apk 

ENV CXX=clang++
ENV CC=clang
ENV LDFLAGS='-L/usr/include -L/usr/include/llvm13'
ENV CXXFLAGS=" -I/usr/include -I/usr/include/llvm13"
ENV PATH="/usr/glibc-compat/bin/:/usr/bin:/usr/local/bin:/zig/bin:$PATH"


COPY . /output
WORKDIR /output
ARG TAG "jul7-2"

# Compile zig
RUN  mkdir build; cd build; cmake .. -DCMAKE_BUILD_TYPE=Release && \
    make -j$(nproc) && \
    rm -rf .git; 

FROM scratch as artifact

COPY --from=zig /output/build /
COPY --from=zig /output/lib /lib