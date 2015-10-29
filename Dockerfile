FROM ubuntu:precise

ENV LANG=en_US.UTF-8

RUN apt-get update -qq \
    && apt-get install -y libmagickwand-dev libwxbase2.8-0 libwxgtk2.8-0 git
RUN curl -L "http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_1_general/esl-erlang_18.1-1~ubuntu~precise_amd64.deb" > erlang.deb \
    && dpkg -i ./erlang.deb \
    && ldconfig

COPY . /opt/memebot/
WORKDIR /opt/memebot
RUN ./rebar3 as prod release
ENTRYPOINT ["/opt/memebot/_build/prod/rel/memebot/bin/memebot"]
CMD ["start"]