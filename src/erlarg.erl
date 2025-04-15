-module(erlarg).

-export([parse/2]).

-include("erlarg.hrl").

-define(REV(List), lists:reverse(List)).

-type type() :: int | string.
-type args() :: [string()].
-type param() :: #param{}.
-type spec() :: map().


parse(Args, Spec = #{ syntax := Syntax }) ->
    case parse(Spec, Syntax, Args, []) of
        {error, _} = Error -> Error;
        {Tree, []} -> {ok, ?REV(Tree)};
        {Tree, Remaining} -> {ok, Tree, Remaining}
    end;
parse(Args, _) ->
    Args.

parse(_, [], Args, Acc) ->
    {Acc, Args};
parse(_, _, [], Acc) ->
    {Acc, []};
parse(Spec, {any, Syntax}, Args, Acc) ->
    case first(Spec, Syntax, Args, Acc) of
        nomatch -> {Acc, Args};
        {Acc2, Args2} -> parse(Spec, {any, Syntax}, Args2, Acc2)
    end;
parse(Spec, {first, Syntax}, Args, Acc) ->
    case first(Spec, Syntax, Args, Acc) of
        nomatch -> {error, nomatch};
        {Acc2, Args2} -> {Acc2, Args2}
    end;
parse(Spec, [Item | Syntax], Args, Acc) when is_list(Syntax) ->
    io:fwrite("item: ~p -> acc: ~p~n", [Item, Acc]),
    case parse(Spec, Item, Args, Acc) of
        {error, _} = Error -> Error;
        {Acc2, Args2} -> parse(Spec, Syntax, Args2, Acc2)
    end;
parse(Spec, Item, Args, Acc) ->
    io:fwrite("item: ~p~n", [Item]),
    case find_param(Spec, Item, Args) of
        {ok, Type, Args2} ->
            io:fwrite("param: ~p~n", [Type]),
            case consume(Spec, Type, Args2) of
                {noparam, Args3} ->
                    {[Item | Acc], Args3};
                {ok, Value, Args3} ->
                    {[Value | Acc], Args3};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error -> Error;
        nomatch -> nomatch
    end.


first(_, [], _, _) ->
    nomatch;
first(Spec, [Item | Syntax], Args, Acc) ->
    case parse(Spec, Item, Args, Acc) of
        {error, _} -> first(Spec, Syntax, Args, Acc);
        nomatch -> first(Spec, Syntax, Args, Acc);
        {Acc2, Args2} -> {Acc2, Args2}
    end.


-spec find_param(Spec, Item, Args) -> {ok, Param, Args} | Error when
      Spec :: spec(),
      Item :: {any(), type()} | type() | atom(),
      Args :: args(),
      Param :: param() | type() | {any(), type()},
      Error :: nomatch | {error, any()}.

find_param(Spec, {Name, Type}, Args) ->
    case find_param(Spec, Type, Args) of
        {ok, Param, Args2} -> {ok, {Name, Param}, Args2};
        nomatch ->
            nomatch;
        {error, _} = Error -> Error
    end;
find_param(_, Type, Args) when Type =:= int; Type =:= string ->
    {ok, Type, Args};
find_param(Spec, Param_name, [_ | _] = Args) when is_atom(Param_name) ->
    case get_param(Spec, Param_name) of
        {ok, #param{} = Param} ->
            case param_matches(Param, Args) of
                {true, Args2} ->
                    {ok, {Param_name, Param}, Args2};
                false ->
                    nomatch
            end;
        error ->
            {error, {undef_param, Param_name}}
    end.

get_param(#{ parameters := Params }, Name) ->
    maps:find(Name, Params);
get_param(_, _) ->
    error.

param_matches(#param{ short = undefined, long = undefined }, Args) ->
    {true, Args};
param_matches(#param{ short = Short, long = Long }, [Arg | Args])
  when Arg =:= Short; Arg =:= Long ->
    {true, Args};
param_matches(Param, [Arg | Args]) ->
    case string:split(Arg, "=") of
        [Left, Value] ->
            param_matches(Param, [Left, Value | Args]);
        [_] ->
            false
    end.


-spec consume(Spec, Type, Args) -> Result | Error when
      Spec :: spec(),
      Type :: any(),
      Args :: args(),
      Result :: {ok, any(), args()},
      Error :: {error, any()}.

consume(_, #param{ syntax = undefined, param = undefined }, Args) ->
    {noparam, Args};
consume(Spec, {Name, Type}, Args) ->
    case consume(Spec, Type, Args) of
        {noparam, Args2} -> {noparam, Args2};
        {ok, Value, Args2} -> {ok, {Name, Value}, Args2};
        {error, Error} -> {error, {Name, Error}}
    end;
consume(_, _, []) ->
    {error, missing_arg};
consume(_, int, [Arg | Args]) ->
    case to_int(Arg) of
        {ok, Int} -> {ok, Int, Args};
        _ -> {error, {badarg, Arg}}
    end;
consume(_, string, [String | Args]) ->
    {ok, String, Args};
consume(Spec, #param{ syntax = undefined, param = Params }, Args) ->
    consume(Spec, Params, Args);
consume(Spec, #param{ syntax = Syntax }, Args) ->
    case parse(Spec, Syntax, Args, []) of
        {error, _} = Error -> Error;
        {Value, Args2} -> {ok, ?REV(Value), Args2}
    end;
consume(Spec, Params, Args) when is_list(Params) ->
    consume(Spec, Params, Args, []);
consume(#{ types := Types }, Type, Args) ->
    case maps:find(Type, Types) of
        {ok, Fun} when is_function(Fun, 1) ->
            case Fun(Args) of
                {error, Error} ->
                    {error, Error};
                {Value, Args2} ->
                    {ok, Value, Args2}
            end;
        error ->
            {error, {undef_type, Type}}
    end;
consume(_, Type, _) ->
    {error, {undef_type, Type}}.

consume(_, [], Args, Acc) ->
    {ok, ?REV(Acc), Args};
consume(Spec, [Param | Params], Args, Acc) ->
    case consume(Spec, Param, Args) of
        {ok, Value, Args2} ->
            consume(Spec, Params, Args2, [Value | Acc]);
        Error ->
            Error
    end.

to_int(String) ->
    case catch list_to_integer(String) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> error
    end.

