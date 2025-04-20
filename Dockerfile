FROM alpine:3.21.2

RUN apk add --no-cache \
        curl \
        erlang \
        erlang-dev \

# rebar3
RUN curl -sL -O https://github.com/erlang/rebar3/releases/download/3.24.0/rebar3 -o /usr/bin/rebar3 && \
    chmod +x /usr/bin/rebar3

# elvis
RUN curl -sL -O https://github.com/inaka/elvis/releases/download/4.0.0/elvis -o /usr/bin/elvis && \
    chmod +x /usr/bin/elvis

