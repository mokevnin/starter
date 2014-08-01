REBAR="./rebar"
RELX="./relx"

all: clean get-deps compile makedir

pull-image:
	docker pull mokevnin/starter

build-image:
	docker build --rm -t="mokevnin/starter" ./priv

clean:
	$(REBAR) clean

release: clean cleanrel compile
	$(RELX)

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

cleanrel:
	rm -rf _rel

cleanapp:
	$(REBAR) clean skip_deps=true

compileapp:
	$(REBAR) compile skip_deps=true

test: compileapp
	ERL_FLAGS="-config $(CURDIR)/sys" $(REBAR) eu skip_deps=true

run: cleanapp compileapp
	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin -s starter

# macrun: cleanapp compileapp
# 	boot2docker start
# 	DOCKER_HOST=tcp://$(boot2docker ip 2>/dev/null):2375
# 	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin -s starter

makedir:
	if [ ! -d /var/tmp/starter ]; then mkdir "/var/tmp/starter"; fi

.PHONY: test
