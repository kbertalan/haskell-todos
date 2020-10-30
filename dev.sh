#!/usr/sbin/env bash

function cleanup()
{
  set -x
  kill -2 $(cat respawn.pid)
  rm respawn.pid
  docker-compose down
}

trap cleanup EXIT

docker-compose up -d
sleep 1
stack build --test --copy-bins --file-watch --exec "respawn.sh haskell-todos-exe" --exec "hlint src test"

