FROM phusion/baseimage:noble-1.0.2
LABEL org.opencontainers.image.authors="Shon Feder <shon@tarides.com>"

RUN apt -y update
RUN apt -y install npm

RUN npm install -g tree-sitter-cli

WORKDIR /tree-sitter-gosu

CMD [ "tree-sitter", "generate" ]
