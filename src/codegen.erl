-module(codegen).

-export([make_forms/2]).


make_forms(ElmMod, Defs) ->
    lists:map(fun (Def) -> to_function(ElmMod, Def) end, Defs).



%% TOP-LEVEL


to_function(ModuleName, {'Def', {'PVar', Name}, Body}) ->
    { cerl:c_fname(qualifed_var(ModuleName, Name), 0),
      cerl:c_fun([], to_expr(Body))
    }.



%% EXPRESSIONS


to_expr({'ELit', Literal}) -> literal(Literal).



literal({'IntNum', N}) -> cerl:c_int(N).



%% HELPERS


qualifed_var(ModuleName, Name) ->
    Mod = module_to_binary(ModuleName),
    binary_to_atom(<<Mod/binary, "@", Name/binary>>, utf8).


module_to_binary({'ModuleName', User, Project, Mod}) ->
    <<User/binary, "@", Project/binary, "@", Mod/binary>>.
