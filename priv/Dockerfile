FROM      ubuntu:saucy
MAINTAINER mokevnin

RUN apt-get update

RUN apt-get install -y python-software-properties software-properties-common g++ make

RUN add-apt-repository ppa:chris-lea/node.js

RUN apt-get update

RUN apt-get install -y ruby1.9.3
RUN apt-get install -y nodejs
RUN apt-get install -y php5
RUN apt-get install -y erlang
