# Build chordemux in Docker

from ubuntu:18.04

run apt -y update && apt -y upgrade

run apt -y install wget curl

run wget -qO- https://get.haskellstack.org/ | sh

run mkdir -p /build-chordemux

run apt install -y git libasound2-dev pkg-config

add . /build-chordemux

workdir /build-chordemux

run git clone https://github.com/selectel/yaml-config

run stack install

run mkdir /out


