REBAR=./rebar

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

