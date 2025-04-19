-module(erlarg).

-export([parse/2]).
-export([opt/1, opt/2, opt/3]).

-record(opt, { short, long, syntax, doc }).

-define(REV(List), lists:reverse(List)).

-type args() :: [string()].
-type base_type() :: int | float | number | bool | string | binary | atom.
-type type() :: base_type() | atom().
-type spec() :: map().

-opaque opt() :: #opt{}.
-opaque syntax() :: [syntax()] | {any | first, [syntax()]}
                  | type() | {any(), type()}.

-export_type([opt/0, syntax/0,
              spec/0, args/0
             ]).


-spec opt(Command) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Param :: opt().

opt(Command) ->
    opt(Command, undefined, undefined).

-spec opt(Command, Syntax) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Syntax :: syntax() | undefined,
      Param :: opt().

opt(Command, Syntax) ->
    opt(Command, Syntax, undefined).


-spec opt(Command, Syntax, Doc) -> Param when
      Command :: string() | undefined
                 | {string() | undefined, string() | undefined},
      Syntax :: syntax() | undefined,
      Doc :: string() | binary() | undefined,
      Param :: opt().

opt({Short, Long}, Syntax, Doc) ->
    #opt{ short = Short, long = Long,
            syntax = Syntax,
            doc = Doc };
opt(Short, Syntax, Doc) ->
    opt({Short, undefined}, Syntax, Doc).


-spec to_int(String) -> Result when
      String :: string(),
      Result :: integer().

to_int(String) ->
    try list_to_integer(String) of
        Int -> Int
    catch
        error:_ -> error({not_int, String})
    end.

-spec to_float(String) -> Result when
      String :: string(),
      Result :: float().

to_float(String) ->
    try list_to_float(String) of
        Float -> Float
    catch
        error:_ ->
            try list_to_integer(String) of
                Int -> float(Int)
            catch
                error:_ ->
                    error({not_float, String})
            end
    end.


-spec to_number(String) -> Result when
      String :: string(),
      Result :: integer() | float().

to_number(String) ->
    try to_int(String) of
        Int -> Int
    catch
        error:{not_int, _} ->
            try to_float(String) of
                Float -> Float
            catch
                error:{not_float, _} ->
                    error({not_number, String})
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
    try to_number(Arg) of
        Float when is_float(Float) -> Float =/= 0.0;
        Int when is_integer(Int) -> Int =/= 0
    catch
        error:_ -> true
    end.


-spec parse(Args, Syntax) -> Options | Error when
      Args :: args(),
      Syntax :: map() | syntax(),
      Options :: {ok, {any(), args()}},
      Error :: error.

parse(Args, #{ syntax := Syntax } = Specs) ->
    try parse(Specs, Syntax, Args, []) of
        {Acc, RemainingArgs} ->
            {ok, {?REV(Acc), RemainingArgs}}
    catch
        error:Error ->
            {error, Error}
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
        _ -> error({missing, arg})
    end;
parse(Specs, {any, Syntax}, Args, Acc) when is_list(Syntax) ->
    case parse(Specs, {first, Syntax}, Args, Acc) of
        {Acc, Args} -> {Acc, Args};
        {Acc2, Args2} -> parse(Specs, {any, Syntax}, Args2, Acc2)
    end;
parse(Specs, {any, Syntax}, Args, Acc) ->
    parse(Specs, {any, [Syntax]}, Args, Acc);
parse(_, {first, []}, Args, Acc) ->
    {Acc, Args};
parse(Specs, {first, [Item | Syntax]}, Args, Acc) ->
    try parse(Specs, Item, Args, Acc) of
        {Acc2, Args2} -> {Acc2, Args2}
    catch
        error:_ ->
            parse(Specs, {first, Syntax}, Args, Acc)
    end;
parse(Specs, [Item | Syntax], Args, Acc) ->
    {Acc2, Args2} = parse(Specs, Item, Args, Acc),
    parse(Specs, Syntax, Args2, Acc2);
parse(Specs, {Name, Syntax}, Args, Acc) ->
    {Value, Args2} = parse(Specs, Syntax, Args, []),
    {[commit_value(Syntax, Name, Value) | Acc], Args2};
parse(_, BaseType, [Arg | Args], Acc)
  when BaseType =:= int; BaseType =:= float; BaseType =:= number;
       BaseType =:= bool; BaseType =:= string; BaseType =:= binary;
       BaseType =:= atom ->
    {[consume(BaseType, Arg) | Acc], Args};
parse(Specs, ParamName, Args, Acc)
  when is_atom(ParamName) ->
    case maps:find(ParamName, maps:get(definitions, Specs, #{})) of
        {ok, #opt{ syntax = Syntax } = Param} ->
            {Value, Args2} = parse(Specs, Param, Args, []),
            {[commit_value(Syntax, ParamName, Value) | Acc], Args2};
        {ok, Fun} when is_function(Fun, 1) ->
            parse(Specs, Fun, Args, Acc);
        error ->
            error({unknown_type, ParamName})
    end;
parse(Specs, #opt{ short = Short, long = Long } = Param, [Arg | Args], Acc)
  when Arg =:= Short; Arg =:= Long ->
    parse(Specs, Param#opt.syntax, Args, Acc);
parse(Specs, #opt{ short = Same, long = Same } = Param, Args, Acc)
  when Same =:= undefined ->
    parse(Specs, Param#opt.syntax, Args, Acc);
parse(Specs, #opt{ long = Long } = Param, [Arg | Args], Acc) ->
    case string:split(Arg, "=") of
        [Long, Value] ->
            parse(Specs, Param, [Long, Value | Args], Acc);
        _ -> error({unhandled, Arg})
    end;
parse(_, Fun, Args, Acc)
  when is_function(Fun, 1) ->
    case Fun(Args) of
        {ok, Value, Args2} -> {[Value | Acc], Args2};
        Error -> error(Error)
    end.

commit_value(_, Name, [novalue]) -> Name;
commit_value(Syntax, Name, [Value])
  when not is_list(Syntax) ->
    {Name, Value};
commit_value(_, Name, Values)
  when is_list(Values) ->
    {Name, ?REV(Values)}.

consume(int, Arg) ->
    to_int(Arg);
consume(float, Arg) ->
    to_float(Arg);
consume(number, Arg) ->
    to_number(Arg);
consume(bool, String) ->
    to_bool(string:lowercase(String));
consume(atom, String) ->
    list_to_atom(String);
consume(binary, String) ->
    unicode:characters_to_binary(String);
consume(string, String) ->
    String.
