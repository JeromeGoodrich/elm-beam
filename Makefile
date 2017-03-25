test:
	@mkdir -p ebin
	@erlc -o ebin src/*.erl tests/*.erl
	@erl -noshell -pa ebin \
	   	-eval "eunit:test(codegen_tests, [verbose])" \
		-run init stop
