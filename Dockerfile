FROM aeternity/builder:1804 as builder

ADD . /app
ENV ERLANG_ROCKSDB_OPTS "-DWITH_SYSTEM_ROCKSDB=ON -DWITH_LZ4=ON -DWITH_SNAPPY=ON -DWITH_BZ2=ON -DWITH_ZSTD=ON"

RUN apt-get -qq update \
    && apt-get -qq -y install git cmake clang curl libsodium23 libgmp10 \
    libsnappy1v5 liblz4-1 liblz4-dev libzstd1 libgflags2.2 libbz2-1.0 \
    && ldconfig \
    && rm -rf /var/lib/apt/lists/*

# Install shared rocksdb code from builder container
RUN ln -fs librocksdb.so.6.13.3 /usr/local/lib/librocksdb.so.6.13 \
    && ln -fs librocksdb.so.6.13.3 /usr/local/lib/librocksdb.so.6 \
    && ln -fs librocksdb.so.6.13.3 /usr/local/lib/librocksdb.so \
    && ldconfig

WORKDIR /app
RUN cp node/rebar3 .
RUN make

CMD ["escript", "aerepl"]

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 8137
