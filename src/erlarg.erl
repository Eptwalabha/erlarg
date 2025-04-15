-module(erlarg).

-export([parse/2]).
-export([param/3]).

-include("erlarg.hrl").

-define(REV(List), lists:reverse(List)).

-type args() :: [string()].
-type base_type() :: int | string.
-type type() :: base_type() | atom().
-type spec() :: map().

-opaque param() :: #param{}.
-opaque syntax() :: [syntax()] | {any | first, [syntax()]}
                  | type() | {any(), type()}.

-export_type([param/0, syntax/0,
              spec/0, args/0
             ]).

-spec param(Command, Syntax, Doc) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Syntax :: syntax(),
      Doc :: string() | binary(),
      Param :: param().

param({Short, Long}, Syntax, Doc) ->
    #param{ short = Short, long = Long,
            syntax = Syntax,
            doc = Doc };
param(Short, Syntax, Doc) ->
    param({Short, undefined}, Syntax, Doc).


-spec to_int(String) -> Result when
      String :: string(),
      Result :: {ok, integer()} | error.

to_int(String) ->
    try list_to_integer(String) of
        Int -> {ok, Int}
    catch
        error:_ -> error
    end.

parse(Args, #{ syntax := Syntax } = Specs) ->
    case parse(Specs, Syntax, Args, []) of
        none -> none;
        {Acc, []} -> {ok, ?REV(Acc)}
    end;
parse(Args, _) ->
    {ok, Args}.


parse(_, [], Args, Acc) ->
    {Acc, Args};
parse(_, _, [], Acc) ->
    {Acc, []};
parse(Specs, {any, Syntax}, Args, Acc) ->
    case parse(Specs, {first, Syntax}, Args, Acc) of
        none -> {Acc, Args};
        {Acc2, Args2} -> parse(Specs, {any, Syntax}, Args2, Acc2)
    end;
parse(_, {first, []}, _, _) ->
    none;
parse(Specs, {first, [Item | Syntax]}, Args, Acc) ->
    case parse(Specs, Item, Args, Acc) of
        none -> parse(Specs, {first, Syntax}, Args, Acc);
        {Acc2, Args2} ->
            {Acc2, Args2}
    end;
parse(Specs, [Item | Syntax], Args, Acc) ->
    case parse(Specs, Item, Args, Acc) of
        none -> none;
        {Acc2, Args2} -> parse(Specs, Syntax, Args2, Acc2)
    end;
parse(Specs, {Name, Syntax}, Args, Acc) ->
    case parse(Specs, Syntax, Args, []) of
        {Value, Args2} ->
            {[commit_value(Syntax, Name, Value) | Acc], Args2}; 
        none -> none
    end;
parse(Specs, BaseType, Args, Acc)
  when BaseType =:= int; BaseType =:= string;
       BaseType =:= binary ->
    case consume(Specs, BaseType, Args) of
        {ok, Value, Args2} ->
            {[Value | Acc], Args2};
        {error, _} ->
            none
    end;
parse(Specs, ParamName, Args, Acc)
  when is_atom(ParamName) ->
    case fetch(Specs, ParamName) of
        {param, #param{ syntax = Syntax } = Param} ->
            case parse(Specs, Param, Args, []) of
                {Value, Args2} ->
                    {[commit_value(Syntax, ParamName, Value) | Acc], Args2}; 
                none -> none
            end;
        {type, Fun} when is_function(Fun, 1) ->
            parse(Specs, Fun, Args, Acc);
        not_found ->
            none
    end;
parse(Specs, #param{ short = Short, long = Long } = Param, [Arg | Args], Acc)
  when Arg =:= Short; Arg =:= Long ->
    case Param#param.syntax of
        undefined -> {[novalue | Acc], Args};
        Syntax -> parse(Specs, Syntax, Args, Acc)
    end;
parse(Specs, #param{ short = undefined, long = undefined } = Param, Args, Acc) ->
    case Param#param.syntax of
        undefined -> {[novalue | Acc], Args};
        Syntax -> parse(Specs, Syntax, Args, Acc)
    end;
parse(Specs, #param{ long = Long } = Param, [Arg | Args], Acc) ->
    case string:split(Arg, "=") of
        [Long, Value] ->
            parse(Specs, Param, [Long, Value | Args], Acc);
        _ -> none
    end;
parse(_, Fun, Args, Acc)
  when is_function(Fun, 1) ->
    case Fun(Args) of
        {error, _} -> none;
        {Value, Args2} -> {[Value | Acc], Args2}
    end.

commit_value(_, Name, [novalue]) -> Name;
commit_value(Syntax, Name, [Value])
  when not is_list(Syntax) ->
    {Name, Value};
commit_value(_, Name, Values)
  when is_list(Values) ->
    {Name, ?REV(Values)}.

fetch(#{} = Specs, Name) ->
    case maps:find(Name, maps:get(parameters, Specs, #{})) of
        {ok, #param{} = Param} -> {param, Param};
        _ ->
            case maps:find(Name, maps:get(types, Specs, #{})) of
                {ok, Fun} when is_function(Fun, 1) -> {type, Fun};
                _ -> not_found
            end
    end.

consume(_, int, [Arg | Args]) ->
    case to_int(Arg) of
        {ok, Int} -> {ok, Int, Args};
        _ -> {error, {badarg, Arg}}
    end;
consume(_, binary, [String | Args]) ->
    {ok, list_to_binary(String), Args};
consume(_, string, [String | Args]) ->
    {ok, String, Args}.
