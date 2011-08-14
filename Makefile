ERL=erl
BEAMDIR=./ebin ./deps/*/ebin
REBAR=./rebar

APP_NAME=file_box
HOST_NAME=127.0.0.1

all: clean compile xref

compile:
	@$(REBAR) get-deps compile

xref:
	@$(REBAR) xref

clean: 
	@ $(REBAR) clean

check:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit 

edoc:
	@$(REBAR) doc

boot:
	@ $(ERL) -pa $(BEAMDIR) -name $(APP_NAME)@$(HOST_NAME) \
                 -boot start_sasl \
                 -s $(APP_NAME) start