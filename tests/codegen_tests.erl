-module(codegen_tests).


-include_lib("eunit/include/eunit.hrl").



%% HELPERS


-define(ELM_MODULE, {'ModuleName', <<"elm-lang">>, <<"core">>, <<"Main">>}).

-define(SYNTHETIC_MAIN,
        { cerl:c_fname(main, 0),
          cerl:c_fun([],
                     cerl:c_apply(cerl:c_fname('elm-lang@core@Main@main', 0),
                                  []
                                 )
                    )
        }).


expect_main_to_yield(Expected, Defs) ->
    { MainName, _ } = Main = ?SYNTHETIC_MAIN,
    Functions = [ Main | codegen:make_forms(?ELM_MODULE, Defs) ],
    CModule = cerl:c_module(cerl:c_atom(elm), [ MainName ], [], Functions),
    { ok, _, Bin } = compile:forms(CModule, [ report, verbose, from_core ]),
    { module, Module } = code:load_binary(elm, "elm.beam", Bin),

    ?assertEqual(Expected, Module:main()),

    code:delete(Module).



%% TESTS


simple_integer_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"main">> },
               { 'ELit', { 'IntNum', 5 } }
             }
           ],

    expect_main_to_yield(5, Defs).


simple_float_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"main">> },
               { 'ELit', { 'FloatNum', 25.0 } }
             }
           ],

    expect_main_to_yield(25.0, Defs).


simple_string_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"main">> },
               { 'ELit', { 'Str', <<"Hello, World">> } }
             }
           ],

    expect_main_to_yield(<<"Hello, World">>, Defs).


simple_list_of_chars_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"main">> },
               { 'List',
                 [ { 'ELit', { 'Chr', <<"a">> } },
                   { 'ELit', { 'Chr', <<"b">> } },
                   { 'ELit', { 'Chr', <<"c">> } }
                 ]
               }
             }
           ],

    expect_main_to_yield([$a, $b, $c], Defs).


local_function_call_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"id">> },
               { 'Lambda', { 'PVar', <<"value">> },
                 { 'EVar', { 'Variable', { 'Local' }, <<"value">> }}
               }
             },
             { 'Def',
               { 'PVar', <<"main">> },
               { 'App',
                 { 'EVar', { 'Variable', { 'TopLevel', ?ELM_MODULE }, <<"id">> }
                 },
                 { 'ELit', {'Boolean','true'} }
               }
             }
           ],

    expect_main_to_yield(true, Defs).
