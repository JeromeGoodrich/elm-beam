-module(codegen).

-export([make_forms/2]).


make_forms(ElmMod, Defs) ->
    lists:map(fun (Def) -> from_def(ElmMod, Def) end, Defs).



%% TOP-LEVEL


from_def(ModuleName, {'Def', {'PVar', Name}, Body}) ->
    case is_operator(Name) of
        true  -> { Args, CBody } = collectLambdas(new_env(), Body, []),
                 { cerl:c_fname(qualifed_name(ModuleName, Name), length(Args)),
                   cerl:c_fun(Args, CBody)
                 };
        false -> { cerl:c_fname(qualifed_name(ModuleName, Name), 0),
                   cerl:c_fun([], from_expr(new_env(), Body))
                 }
    end.



collectLambdas(Env, {'Lambda', Pattern, Body}, Acc) ->
    { NewEnv, Arg } = from_pattern(Env, Pattern),
    collectLambdas(NewEnv, Body, Acc ++ [Arg]);

collectLambdas(Env, Body, Acc) ->
    { Acc, from_expr(Env, Body) }.



%% EXPRESSIONS


from_expr(Env, {'ELit', Literal}) ->
    from_literal(Literal);

from_expr(Env, {'EVar', Var}) ->
    from_var(Var);

from_expr(Env, {'Binop', Var, Left, Right}) ->
    Op = case Var of
            {'Variable', {'TopLevel', Home}, Name} ->
                cerl:c_var({ qualifed_name(Home, Name), 2 });
            {'Variable', {'Module', Home}, Name} ->
                cerl:c_var({ qualifed_name(Home, Name), 2 })
        end,
    cerl:c_apply(Op, [from_expr(Env, Left), from_expr(Env, Right)]);

from_expr(Env, {'Lambda', Pattern, Body}) ->
    { NewEnv, P } = from_pattern(Env, Pattern),
    cerl:c_fun([P], from_expr(NewEnv, Body));

from_expr(Env, {'App', Function, Arg}) ->
    cerl:c_apply(from_expr(Env, Function), [from_expr(Env, Arg)]);

from_expr(Env, {'List', Elems}) ->
    F = fun(E, Acc) ->
                cerl:c_cons(from_expr(Env, E), Acc)
        end,
    lists:foldr(F, cerl:c_nil(), Elems).



from_pattern(Env, {'Anything'}) ->
    fresh_var(Env);

from_pattern(Env, {'PVar', Name}) ->
    { Env, cerl:c_var(binary_to_atom(Name, utf8)) }.



from_var({'Variable', {'TopLevel', Home}, Name}) ->
    cerl:c_apply(cerl:c_var({ qualifed_name(Home, Name), 0 }), []);

from_var({'Variable', {'Local'} , Name}) ->
    cerl:c_var(binary_to_atom(Name, utf8)).



from_literal({'Boolean', A}) ->
    cerl:c_atom(A);

from_literal({'IntNum', N}) ->
    cerl:c_int(N);

from_literal({'FloatNum', N}) ->
    cerl:c_float(N);

from_literal({'Chr', <<C, _/binary>>}) ->
    cerl:c_char(C);

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


new_env() ->
    1.


fresh_var(Env) ->
    { Env + 1, cerl:c_var(Env) }.


is_operator(<<First/utf8, _/binary>>) ->
    lists:member(First, "+-/*=.<>:&|^?%~!").


qualifed_name(ModuleName, Name) ->
    Mod = module_to_binary(ModuleName),
    binary_to_atom(<<Mod/binary, "@", Name/binary>>, utf8).


module_to_binary({'ModuleName', User, Project, Mod}) ->
    <<User/binary, "@", Project/binary, "@", Mod/binary>>.
