-module(codegen).

-export([make_forms/2]).


make_forms(ElmMod, Defs) ->
    lists:map(fun (Def) -> from_def(ElmMod, Def) end, Defs).



%% TOP-LEVEL


from_def(ModuleName, {'Def', {'PVar', Name}, Body}) ->
    { cerl:c_fname(qualifed_var(ModuleName, Name), 0),
      cerl:c_fun([], from_expr(Body))
    }.



%% EXPRESSIONS


from_expr({'ELit', Literal}) -> from_literal(Literal);
from_expr({'List', Elems}) ->
    F = fun(E, Acc) ->
                cerl:c_cons(from_expr(E), Acc)
        end,

    lists:foldr(F, cerl:c_nil(), Elems).



from_literal({'Boolean', A}) -> cerl:c_atom(A);
from_literal({'IntNum', N}) -> cerl:c_int(N);
from_literal({'FloatNum', N}) -> cerl:c_float(N);
from_literal({'Chr', <<C, _/binary>>}) -> cerl:c_char(C);
from_literal({'Str', Bin}) ->
    F = fun(I) ->
                cerl:c_bitstr(
                  cerl:c_int(I), cerl:c_int(8), cerl:c_int(1),
                  cerl:c_atom(integer),
                  cerl:c_cons(cerl:c_atom(unsigned),
                              cerl:c_cons(cerl:c_atom(big), cerl:c_nil())))
        end,

    cerl:c_binary([F(I) || I <- binary_to_list(Bin)]).



%% HELPERS


qualifed_var(ModuleName, Name) ->
    Mod = module_to_binary(ModuleName),
    binary_to_atom(<<Mod/binary, "@", Name/binary>>, utf8).


module_to_binary({'ModuleName', User, Project, Mod}) ->
    <<User/binary, "@", Project/binary, "@", Mod/binary>>.
