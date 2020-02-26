#!/bin/bash
#
# Creates an image that runs the locally compiled zzbot

set -e # Exit on command failure
set -u # Exit on read of unset variable

declare -r ZZ_TMPDIR=$(mktemp -d -t zzbot_docker_run-XXXXXXXX)

function run() {
  echo "$@"
  "$@"
}

function on_exit() {
  run rm -Rf "$ZZ_TMPDIR"
}

trap on_exit EXIT

run stack build
declare -r ZZ_EXECUTABLE=$(stack exec which zzbot)
if [ ! -e "$ZZ_EXECUTABLE" ]; then
  echo "zzbot executable not found"
  exit 1
fi
declare -r DOCKERFILE="run/Dockerfile"
if [ ! -e "$DOCKERFILE" ]; then
  echo "$DOCKERFILE doesn't exist. You should execute this script from zzbot/docker";
  exit 1
fi

run cp "$ZZ_EXECUTABLE" "$DOCKERFILE" "../configs/tests/simplest.xml" "$ZZ_TMPDIR/."
run cd "$ZZ_TMPDIR"
run ls
run docker build -t zzbot:1.0 .
