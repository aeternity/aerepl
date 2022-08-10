# Put aeternity node in second stage container
FROM erlang:25.0.3

RUN apt-get update \
    && apt-get install -y cmake clang libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

ADD . /app

WORKDIR /app
RUN make

CMD ["escript", "aerepl"]

# Erl handle SIGQUIT instead of the default SIGINT
STOPSIGNAL SIGQUIT

EXPOSE 8137
