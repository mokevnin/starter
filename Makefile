REBAR="rebar"

all: get-deps compile

pull-image:
	docker pull mokevnin/starter

build-image:
	docker build -rm -t="mokevnin/starter" ./priv

clean:
	$(REBAR) clean

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

cleanapp:
	$(REBAR) clean skip_deps=true

compileapp:
	$(REBAR) compile skip_deps=true

test: compileapp
	ERL_FLAGS="-config $(CURDIR)/sys" $(REBAR) eu skip_deps=true

run: cleanapp compileapp
	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin -s starter

macrun: cleanapp compileapp
	boot2docker start
	export DOCKER_HOST=tcp://localhost:4243
	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin -s starter

.PHONY: test
