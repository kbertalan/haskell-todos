#!/usr/sbin/env bash

function cleanup()
{
  set -x
  docker-compose down
}

trap cleanup EXIT

docker-compose up -d
sleep 1
make devel

