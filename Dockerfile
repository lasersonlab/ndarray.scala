FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
    build-essential \
    python2.7 \
    python-pip

RUN pip install numpy h5py