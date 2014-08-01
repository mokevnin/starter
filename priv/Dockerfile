FROM       ubuntu:trusty
MAINTAINER mokevnin

RUN apt-get update

RUN apt-get install -y python-software-properties software-properties-common g++ make

RUN add-apt-repository ppa:chris-lea/node.js
RUN add-apt-repository ppa:webupd8team/java -y

RUN apt-get update

RUN apt-get install -y ruby
RUN apt-get install -y nodejs
RUN apt-get install -y php5
RUN apt-get install -y erlang

RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
RUN apt-get install -y oracle-java8-installer
