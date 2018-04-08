.ONESHELL:
test-all:
	cd erlang_laser
	rebar3 eunit
	rebar3 dialyzer