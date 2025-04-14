-module(erlarg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlarg.hrl").

parse_any_test_() ->
    Test = fun (Args, Syntax) ->
                   {ok, Tree} = erlarg:parse(Args, spec(Syntax)),
                   Tree
           end,
    [?_assertEqual(["-a", "2"], Test(["-a", "2"], {any, [string]})),
     ?_assertEqual(["-a", "2"], Test(["-a", "2"], {any, [string, int]})),
     ?_assertEqual(["a", 2, "c", 4],
                   Test(["a", "2", "c", "4"], {any, [int, string]})),
     ?_assertEqual(["a", {i, 2}, "c", {i, 4}],
                   Test(["a", "2", "c", "4"], {any, [{i, int}, string, {n, int}]})),
     ?_assertEqual(["-a", 2], Test(["-a", "2"], [string, int])),
     ?_assertEqual(["-a", {a, 2}, {b, "-c"}, 4],
                   Test(["-a", "2", "-c", "4"], [string, {a, int}, {b, string}, int])),
     ?_assertEqual([1, 2, "a", "1"],
                   Test(["1", "2", "a", "1"], [{any, [int]}, {any, [string]}]))

    ].

parse_big_test_() ->
    Test = fun (Args) ->
                   {ok, Tree} = erlarg:parse(Args, spec()),
                   Tree
           end,
    [?_assertEqual([version], Test(["-v"])),
     ?_assertEqual([help, help, {source, stdin}], Test(["-h", "--help", "-"])),
     ?_assertEqual([{filter, [invert, {columns, "a,b"}, {search, "search A"}]},
                    {filter, [invert, {search, "search B"}]},
                    {filter, [{columns, "a,b"}, {search, "search C"}]},
                    {filter, [{search, "search D"}]}],
                   Test(["--grep", "--invert-match", "-c", "a,b", "search A",
                         "-g", "-v", "search B",
                         "-g", "--columns", "a,b", "search C",
                         "--grep=search D"]))
    ].


spec() ->
    spec({any, [limit, filter, delimiter, help, version, {source, stdin}, {source, string}]}).

spec(Syntax) ->
    #{
      syntax => Syntax,

      parameters => #{ limit => #param{ short = "-l", long = "--long", param = int },
                       filter => #param{ short = "-g", long = "--grep",
                                         syntax = [{any, [invert, columns]},
                                                   {search, string}] },
                       invert => #param{ short = "-v", long = "--invert-match" },
                       help => #param{ short = "-h", long = "--help" },
                       version => #param{ short = "-v", long = "--version" },
                       columns => #param{ short = "-c", long = "--columns",
                                          param = columns },
                       delimiter => #param{ short = "-d", long = "--delimiter",
                                            param = delimiter },
                       stdin => #param{ short = "-", long = "--stdin" },
                       source => #param{ syntax = {any, [stdin, {path, string}]} }
                     },

      types => #{ delimiter => fun (["TAB" | Args]) -> {$\t, Args};
                                   ([[Char] | Args]) -> {Char, Args};
                                   ([Arg | _]) -> {error, {bad_delimiter, Arg}}
                               end,
                  columns => fun columns/1
                }
     }.

columns([Item | Args]) ->
    case string:split(Item, "*") of
        [_, _] -> {error, {bad_columns, Item}};
        _ -> {Item, Args}
    end.
