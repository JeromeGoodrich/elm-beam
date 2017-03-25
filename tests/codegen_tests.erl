-module(codegen_tests).

-include_lib("eunit/include/eunit.hrl").



expect_test_to_yield(Expected, File) ->
    Path = "tests/test-files/" ++ File ++ ".elm.erlx",

    { ok, Erlx } = file:read_file(Path),
    { ok, Tokens, _ } = erl_scan:string(binary_to_list(Erlx)),
    { ok, Abs } = erl_parse:parse_exprs(Tokens),
    { value, Defs, _ } = erl_eval:exprs(Abs, []),

    ElmModule = { 'ModuleName', <<"elm-lang">>, <<"core">>, <<"Main">> },
    TestFunction = cerl:c_fname('elm-lang@core@Main@test', 0),
    SyntheticTest = { cerl:c_fname(test, 0),
                      cerl:c_fun([], cerl:c_apply(TestFunction, []))
                    },

    { MainName, _ } = Main = SyntheticTest,
    Functions = [ Main | codegen:make_forms(ElmModule, Defs) ],
    CModule = cerl:c_module(cerl:c_atom(elm), [ MainName ], [], Functions),
    { ok, _, Bin } = compile:forms(CModule, [ report, verbose, from_core ]),
    { module, Module } = code:load_binary(elm, "elm.beam", Bin),

    ?assertEqual(Expected, Module:test()),

    code:delete(Module).



int_test() ->
    expect_test_to_yield(5, "Int").

float_test() ->
    expect_test_to_yield(25.0, "Float").

string_test() ->
    expect_test_to_yield(<<"Hello, World">>, "String").

chars_list_test() ->
    expect_test_to_yield([$a, $b, $c], "CharList").

id_test() ->
    expect_test_to_yield(true, "Id").
