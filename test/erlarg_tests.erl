-module(erlarg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlarg.hrl").

parse_bad_spec_test_() ->
    SpecWithoutSyntax = #{},
    [?_assertEqual({ok, ["a", "b", "c"]},
                   parse(["a", "b", "c"], SpecWithoutSyntax))].

parse_any_test_() ->
    Test = fun (Args, Syntax) ->
                   {ok, Tree} = parse(Args, spec(Syntax)),
                   Tree
           end,
    [?_assertEqual([123], Test(["123"], int)),
     ?_assertEqual(["123"], Test(["123"], string)),
     ?_assertEqual([{toto, 123}], Test(["123"], {toto, int})),
     ?_assertEqual(["-a", "2"], Test(["-a", "2"], {any, [string]})),
     ?_assertEqual(["-a", "2"], Test(["-a", "2"], {any, [string, int]})),
     ?_assertEqual(["a", 2, "c", 4],
                   Test(["a", "2", "c", "4"], {any, [int, string]})),
     ?_assertEqual(["a", {i, 2}, "c", {i, 4}],
                   Test(["a", "2", "c", "4"],
                        {any, [{i, int}, string, {n, int}]})),
     ?_assertEqual(["-a", 2],
                   Test(["-a", "2"],
                        [string, int])),
     ?_assertEqual(["-a", {a, 2}, {b, "-c"}, 4],
                   Test(["-a", "2", "-c", "4"],
                        [string, {a, int}, {b, string}, int])),
     ?_assertEqual([1, 2, "a", "1"],
                   Test(["1", "2", "a", "1"],
                        [{any, [int]}, {any, [string]}])),
     ?_assertEqual([1, {a, 2}, {b, [3]}],
                   Test(["1", "2", "3"],
                        [int, {a, int}, {b, [int]}])),
     ?_assertEqual([<<"1">>, {a, <<"2">>}, {b, [<<"3">>]}],
                   Test(["1", "2", "3"],
                        [binary, {a, binary}, {b, [binary]}]))
    ].

multi_params_test_() ->
    Spec = #{ syntax => [a, b],
              parameters => #{
                a => #param{ short = "-a", syntax = [string, int]},
                b => #param{ short = "-b",
                             syntax = [{item1, string}, {item2, string}]}
               }
            },
    Test = fun (Args) ->
                   {ok, Tree} = parse(Args, Spec),
                   Tree
           end,
    [?_assertEqual([{a, ["abc", 1]}, {b, [{item1, "def"}, {item2, "2"}]}],
                   Test(["-a", "abc", "1", "-b", "def", "2"]))
    ].


parse_big_test_() ->
    Test = fun (Args) ->
                   {ok, Tree} = parse(Args, spec()),
                   Tree
           end,
    [?_assertEqual([version], Test(["-v"])),
     ?_assertEqual([help, help, {source, [stdin, {path, "toto.tsv"}]}],
                   Test(["-h", "--help", "-", "toto.tsv"])),
     ?_assertEqual([{filter, [invert, {columns, "a,b"}, {search, "search A"}]},
                    {filter, [invert, {search, "search B"}]},
                    {filter, [{columns, "a,b"}, {search, "search C"}]},
                    {filter, [{search, "search D"}]},
                    {source, [{path, "toto"}]}
                   ],
                   Test(["--grep", "--invert-match", "-c", "a,b", "search A",
                         "-g", "-v", "search B",
                         "-g", "--columns", "a,b", "search C",
                         "--grep=search D", "toto"]))
    ].

parse_p_test_() ->
    Spec = #{ parameters => #{
                a => param({"-a", "--a-long"}, {first, [int, string]}),
                b => param("-b", [{any, [a, [{b, int}, {bs, string}]]},
                                  c]),
                c => param(undefined, int)
               }
            },
    Test = fun (Args, Syntax) ->
                   {ok, Tree} = parse(Args, Spec#{ syntax => Syntax }),
                   Tree
           end,
    [?_assertEqual([{a, 1}, {b, [{b, 2}, {bs, "3"}]}],
                   Test(["--a-long=1", "-b", "2", "3"], [a, b]))
    ].

param(Key, Syntax) ->
    erlarg:param(Key, Syntax, <<"döc"/utf8>>).

parse(Args, Spec) ->
    erlarg:parse(Args, Spec).


spec() ->
    spec({any, [limit, filter, delimiter_in, delimiter_out,
                help, version, source]}).

spec(Syntax) ->
    #{
      syntax => Syntax,

      parameters => #{
                       limit => erlarg:param({"-l", "--long"},
                                             int,
                                             "nbr of lines to display"),
                       filter => erlarg:param({"-g", "--grep"},
                                              [{any, [invert, columns]},
                                               {search, string}],
                                              "filter line"),
                       invert => erlarg:param({"-v", "--invert-match"},
                                              undefined,
                                              "invert the matching selection"),
                       help => erlarg:param({"-h", "--help"},
                                            undefined,
                                            "print this help"),
                       version => erlarg:param({"-v", "--version"}, undefined,
                                               "print current version"),
                       columns => param({"-c", "--columns"}, type_columns),
                       delimiter_in => param({"-d", "--delimiter"},
                                             type_delimiter),
                       delimiter_out => param({undefined, "--out-delimiter"},
                                              type_delimiter),
                       stdin => erlarg:param({"-", "--stdin"}, undefined,
                                             "fetch data from stdin"),
                       source => erlarg:param(undefined,
                                              [{any, [stdin, {path, string}]}],
                                              "data source")
                     },

      types => #{ type_delimiter => fun (["TAB" | Args]) -> {$\t, Args};
                                   ([[Char] | Args]) -> {Char, Args};
                                   ([Arg | _]) -> {error, {bad_delimiter, Arg}}
                               end,
                  type_columns => fun columns/1
                }
     }.

columns([Item | Args]) ->
    case string:split(Item, "*") of
        [_, _] -> {error, {bad_columns, Item}};
        _ -> {Item, Args}
    end.
