-module(codegen_tests).


-include_lib("eunit/include/eunit.hrl").



%% HELPERS


-define(ELM_MODULE, {'ModuleName', <<"elm-lang">>, <<"core">>, <<"Main">>}).

-define(MAIN_FUNCTION, 'elm-lang@core@Main@main').


map_compiled(Callback, Defs) ->
    { MainName, MainBody } = Main = synthetic_main(),
    Functions = [ Main | codegen:make_forms(?ELM_MODULE, Defs) ],
    CModule = cerl:c_module(cerl:c_atom(elm), [ MainName ], [], Functions),
    { ok, _, Bin } = compile:forms(CModule, [ report, verbose, from_core ]),
    { module, Module } = code:load_binary(elm, "elm.beam", Bin),

    Callback(Module),

    true = code:delete(Module).


synthetic_main() ->
    { cerl:c_fname(main, 0),
      cerl:c_fun([], cerl:c_apply(cerl:c_fname(?MAIN_FUNCTION, 0), []))
    }.



%% TESTS


simple_integer_test() ->
    Defs = [ { 'Def',
               { 'PVar', <<"main">> },
               { 'ELit', { 'IntNum', 5 } }
             }
           ],

    Test = fun (Module) ->
                   ?assertEqual(5, Module:main())
           end,

    map_compiled(Test, Defs).
