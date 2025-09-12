#!/usr/bin/env sh

docker run --mount type=bind,src=$PWD,dst=/tree-sitter-gosu -it $(docker build -q .) "$@"
