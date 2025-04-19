-module(erlarg).

-export([parse/2]).
-export([param/1, param/2, param/3]).

-record(param, { short, long, syntax, doc }).

-define(REV(List), lists:reverse(List)).

-type args() :: [string()].
-type base_type() :: int | float | number | bool | string | binary | atom.
-type type() :: base_type() | atom().
-type spec() :: map().

-opaque param() :: #param{}.
-opaque syntax() :: [syntax()] | {any | first, [syntax()]}
                  | type() | {any(), type()}.

-export_type([param/0, syntax/0,
              spec/0, args/0
             ]).


-spec param(Command) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Param :: param().

param(Command) ->
    param(Command, undefined, undefined).

-spec param(Command, Syntax) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Syntax :: syntax() | undefined,
      Param :: param().

param(Command, Syntax) ->
    param(Command, Syntax, undefined).


-spec param(Command, Syntax, Doc) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Syntax :: syntax() | undefined,
      Doc :: string() | binary() | undefined,
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

-spec to_float(String) -> Result when
      String :: string(),
      Result :: {ok, float()} | error.

to_float(String) ->
    try list_to_float(String) of
        Float -> {ok, Float}
    catch
        error:_ -> error
    end.


-spec to_number(String) -> Result when
      String :: string(),
      Result :: {ok, integer() | float()} | error.

to_number(String) ->
    case to_int(String) of
        {ok, Int} -> {ok, Int};
        _ ->
            case to_float(String) of
                {ok, Float} -> {ok, Float};
                _ -> error
            end
    end.


to_bool("") -> false;
to_bool("0") -> false;
to_bool("disabled") -> false;
to_bool("f") -> false;
to_bool("false") -> false;
to_bool("n") -> false;
to_bool("no") -> false;
to_bool(Arg) ->
    case to_number(Arg) of
        {ok, Float} when is_float(Float) -> Float =/= 0.0;
        _ -> true
    end.

parse(Args, #{ syntax := Syntax } = Specs) ->
    case parse(Specs, Syntax, Args, []) of
        none -> none;
        {Acc, []} -> {ok, ?REV(Acc)}
    end;
parse(Args, Syntax) ->
    parse(Args, #{ syntax => Syntax }).


parse(_, undefined, Args, Acc) ->
    {[novalue | Acc], Args};
parse(_, [], Args, Acc) ->
    {Acc, Args};
parse(_, Syntax, [], Acc) ->
    case Syntax of
        {any, _} -> {Acc, []};
        _ -> none
    end;
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
  when BaseType =:= int; BaseType =:= float; BaseType =:= number;
       BaseType =:= bool; BaseType =:= string; BaseType =:= binary;
       BaseType =:= atom ->
    case consume(Specs, BaseType, Args) of
        {ok, Value, Args2} ->
            {[Value | Acc], Args2};
        {error, _} ->
            none
    end;
parse(Specs, ParamName, Args, Acc)
  when is_atom(ParamName) ->
    case maps:find(ParamName, maps:get(definitions, Specs, #{})) of
        {ok, #param{ syntax = Syntax } = Param} ->
            case parse(Specs, Param, Args, []) of
                {Value, Args2} ->
                    {[commit_value(Syntax, ParamName, Value) | Acc], Args2};
                none -> none
            end;
        {ok, Fun} when is_function(Fun, 1) ->
            parse(Specs, Fun, Args, Acc);
        error ->
            none
    end;
parse(Specs, #param{ short = Short, long = Long } = Param, [Arg | Args], Acc)
  when Arg =:= Short; Arg =:= Long ->
    parse(Specs, Param#param.syntax, Args, Acc);
parse(Specs, #param{ short = Same, long = Same } = Param, Args, Acc)
  when Same =:= undefined ->
    parse(Specs, Param#param.syntax, Args, Acc);
parse(Specs, #param{ long = Long } = Param, [Arg | Args], Acc) ->
    case string:split(Arg, "=") of
        [Long, Value] ->
            parse(Specs, Param, [Long, Value | Args], Acc);
        _ -> none
    end;
parse(_, Fun, Args, Acc)
  when is_function(Fun, 1) ->
    case Fun(Args) of
        {ok, Value, Args2} -> {[Value | Acc], Args2};
        _ -> none
    end.

commit_value(_, Name, [novalue]) -> Name;
commit_value(Syntax, Name, [Value])
  when not is_list(Syntax) ->
    {Name, Value};
commit_value(_, Name, Values)
  when is_list(Values) ->
    {Name, ?REV(Values)}.

consume(_, int, [Arg | Args]) ->
    case to_int(Arg) of
        {ok, Int} -> {ok, Int, Args};
        _ -> {error, {badarg, Arg}}
    end;
consume(_, float, [Arg | Args]) ->
    case to_number(Arg) of
        {ok, Int} when is_integer(Int) -> {ok, float(Int), Args};
        {ok, Float} when is_float(Float) -> {ok, Float, Args};
        _ -> {error, {badarg, Arg}}
    end;
consume(_, number, [Arg | Args]) ->
    case to_number(Arg) of
        {ok, Number} -> {ok, Number, Args};
        _ -> {error, {badarg, Arg}}
    end;
consume(_, bool, [String | Args]) ->
    {ok, to_bool(string:lowercase(String)), Args};
consume(_, atom, [String | Args]) ->
    {ok, list_to_atom(String), Args};
consume(_, binary, [String | Args]) ->
    {ok, unicode:characters_to_binary(String), Args};
consume(_, string, [String | Args]) ->
    {ok, String, Args}.
