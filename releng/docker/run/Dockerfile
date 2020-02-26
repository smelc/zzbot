# Docker image that installs zzbot executable into an image and starts it
# FROM alpine:3.7 <- ran into weird issues with this one :-(
FROM ubuntu:18.04

RUN apt-get update && apt install -y lsof

COPY zzbot .
COPY simplest.xml .
RUN pwd
RUN ls

# Command doesn't return, intentionally
ENTRYPOINT ["/zzbot", "--web", "simplest.xml"]
