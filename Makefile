REBAR="rebar"

all: get-deps compile

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

run: cleanapp compileapp
	erl -config $(CURDIR)/sys -pa ebin deps/*/ebin
