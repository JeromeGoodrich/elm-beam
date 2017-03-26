.PHONY: bootstrap test

test:
	@elm-beam-bootstrap tests/test-files
	@mkdir -p ebin
	@erlc -o ebin src/*.erl tests/*.erl
	@erl -noshell -pa ebin \
	   	-eval "eunit:test(codegen_tests, [verbose])" \
		-run init stop

bootstrap:
	git submodule update --init
	cd bootstrap && stack install
