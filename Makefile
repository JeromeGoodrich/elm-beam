test:
	@for elm in tests/test-files/*.elm; do elm-beam-bootstrap $$elm; done
	@mkdir -p ebin
	@erlc -o ebin src/*.erl tests/*.erl
	@erl -noshell -pa ebin \
	   	-eval "eunit:test(codegen_tests, [verbose])" \
		-run init stop

bootstrap:
	git submodule update
	cd bootstrap && stack install
