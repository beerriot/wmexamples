ERL          ?= erl
APP          := wmexamples

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@$(ERL) -noshell -run edoc_run application \
          '$(APP)' '"."' '[{preprocess, true},{includes, ["deps"]}]'

