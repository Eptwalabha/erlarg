-module(erlarg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ERROR(Args, Specs), parse_with_error(Args, Specs)).

parse_syntax_directly_test_() ->
    [?_assertEqual(["a", "b", "c"], parse(["a", "b", "c"], {any, [string]})),
     ?_assertEqual([1, 2, 3], parse(["1", "2", "3"], {any, [int]}))
    ].

parse_basic_types_test_() ->
    [?_assertEqual([123], parse(["123"], int)),
     ?_assertEqual([123.4], parse(["123.4"], float)),
     ?_assertEqual([123.0], parse(["123"], float)),
     ?_assertEqual([123.4], parse(["1.234e2"], float)),
     ?_assertEqual([123], parse(["123"], number)),
     ?_assertEqual([123.4], parse(["123.4"], number)),
     ?_assertEqual([123.4], parse(["1.234e2"], number)),
     ?_assertEqual([-123.4], parse(["-1.234e2"], number)),
     ?_assertEqual(["123"], parse(["123"], string)),
     ?_assertEqual([<<"äbc"/utf8>>], parse(["äbc"], binary)),
     ?_assertEqual(['äbc'], parse(["äbc"], atom)),
     ?_assertEqual([{key, 123}], parse(["123"], {key, int})),
     ?_assertEqual([{key, 123}], parse(["123"], {key, number})),
     ?_assertEqual([{key, 123.4}], parse(["123.4"], {key, float})),
     ?_assertEqual([{key, "123"}], parse(["123"], {key, string})),
     ?_assertEqual([{key, <<"äbc"/utf8>>}], parse(["äbc"], {key, binary})),
     ?_assertEqual([{key, 'äbc'}], parse(["äbc"], {key, atom}))
    ].

parse_boolean_test_() ->
    False = ["false", "f", "0", "0.0000", "no", "n",
             "False", "fAlSe", "disabled", ""],
    True = ["true", "t", "1", "1000", "0.00001", "yes", "y",
            "True", "tRuE", "enabled", "abcd"],
    [[?_assertEqual([false], parse([Arg], bool)) || Arg <- False],
     [?_assertEqual([true], parse([Arg], bool)) || Arg <- True]].

parse_param_test_() ->
    [?_assertEqual([123],
                   parse(["-p", "123"], param("-p", int))),
     ?_assertEqual(["123"],
                   parse(["--param", "123"], param({"-p", "--param"}, string))),
     ?_assertEqual([123.0],
                   parse(["--param=123.0"], param({"-p", "--param"}, float))),
     ?_assertEqual([tag],
                   parse(["-p"], {tag, param({"-p", "--param"})})),
     ?_assertEqual([<<"äbc"/utf8>>],
                   parse(["äbc"], param(undefined, binary)))
    ].

parse_any_test_() ->
    [?_assertEqual(["-a", "2"], parse(["-a", "2"], {any, [string]})),
     ?_assertEqual(["-a", "2"], parse(["-a", "2"], {any, [string, int]})),
     ?_assertEqual(["a", 2, "c", 4],
                   parse(["a", "2", "c", "4"], {any, [int, string]})),
     ?_assertEqual(["a", {i, 2}, "c", {i, 4}],
                   parse(["a", "2", "c", "4"],
                        {any, [{i, int}, string, {n, int}]})),
     ?_assertEqual(["-a", 2],
                   parse(["-a", "2"],
                        [string, int])),
     ?_assertEqual(["-a", {a, 2}, {b, "-c"}, 4],
                   parse(["-a", "2", "-c", "4"],
                        [string, {a, int}, {b, string}, int])),
     ?_assertEqual([1, 2, "a", "1"],
                   parse(["1", "2", "a", "1"],
                         [{any, [int]}, {any, [string]}])),
     ?_assertEqual([1, {a, 2}, {b, [3]}],
                   parse(["1", "2", "3"],
                        [int, {a, int}, {b, [int]}])),
     ?_assertEqual([<<"1">>, {a, <<"2">>}, {b, [<<"3">>]}],
                   parse(["1", "2", "3"],
                        [binary, {a, binary}, {b, [binary]}]))
    ].


parse_first_test_() ->
    [?_assertEqual([1], parse(["1"], {first, [int, string]})),
     ?_assertEqual(["abc"], parse(["abc"], {first, [int, string]})),
     ?_assertEqual([{f, 1.1}], parse(["1.1"], {first, [{i, int}, {f, float}]})),
     ?_assertEqual([{d, [1, "abc"]}],
                   parse(["1", "abc"],
                         {first, [{a, [int, int]},
                                  {b, [float, float]},
                                  {c, [int, string, int]},
                                  {d, [number, string]}]}))
    ].

parse_custom_types_test_() ->
    CustomFun = fun (["boom" | _]) ->
                        fails;
                    ([Arg | Args]) ->
                        {ok, {length(Args), lists:reverse(Arg)}, Args}
                end,
    Spec = fun (Syntax) ->
                   #{ syntax => Syntax,
                      definitions => #{
                        custom => CustomFun
                       }
                    }
           end,
    [?_assertEqual([{0, "cba"}], parse(["abc"], CustomFun)),
     ?_assertEqual([{0, "cba"}], parse(["abc"], Spec(custom))),
     ?_assertEqual([{2, "ba"}, "boom", {0, "dc"}],
                   parse(["ab", "boom", "cd"], Spec({any, [custom, string]})))
    ].

multi_params_test_() ->
    Spec = #{ syntax => [a, b],
              definitions => #{
                a => param("-a", [string, int]),
                b => param("-b", [{item1, string}, {item2, string}])
               }
            },
    [?_assertEqual([{a, ["abc", 1]}, {b, [{item1, "def"}, {item2, "2"}]}],
                   parse(["-a", "abc", "1", "-b", "def", "2"], Spec))
    ].


parse_big_test_() ->
    Test = fun (Args) ->
                   parse(Args, spec())
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
    Spec = #{ definitions => #{
                a => param({"-a", "--a-long"}, {first, [int, string]}),
                b => param("-b", [{any, [a, [{b, int}, {bs, string}]]},
                                  c]),
                c => param(undefined, int)
               }
            },
    Test = fun (Args, Syntax) ->
                   parse(Args, Spec#{ syntax => Syntax })
           end,
    [?_assertEqual([{a, 1}, {b, [{b, 2}, {bs, "3"}, {c, 4}]}],
                   Test(["--a-long=1", "-b", "2", "3", "4"], [a, b]))
    ].

parse_readme_example_test() ->
    Spec = {any, [{limit, erlarg:param({"-l", "--limit"}, int)},
                  {format, erlarg:param({"-f", "--format"}, binary)},
                  {file, erlarg:param("-o", string)},
                  {stdin, erlarg:param("-")},
                  {max, erlarg:param({"-m", "--max"}, float)}]},
    Args = ["--limit=20",
            "-m", "0.25",
            "--format", "%s%t",
            "-o", "output.tsv",
            "-"],
    Expected = [{limit, 20},
                {max, 0.25},
                {format, <<"%s%t">>},
                {file, "output.tsv"},
                stdin],
    ?assertEqual({ok, {Expected, []}}, erlarg:parse(Args, Spec)).


remaining_args_test_() ->
    [?_assertEqual(["b", "c"], remain(["a", "b", "c"], string)),
     ?_assertEqual(["c"], remain(["1", "b", "c"], [int, string])),
     ?_assertEqual([], remain(["a", "b", "c"], {any, string})),
     ?_assertEqual(["2.2", "c"], remain(["1", "2.2", "c"], {any, int})),
     ?_assertEqual(["c"], remain(["1", "2.2", "c"], {any, float}))
    ].


error_test_() ->
    [?_assertEqual({missing, arg}, ?ERROR(["-a"], param("-a", int))),
     ?_assertEqual({not_int, "a"}, ?ERROR(["1", "a"], [int, int])),
     ?_assertEqual({not_int, "1.2"}, ?ERROR(["a", "1.2"], [string, int])),
     ?_assertEqual({not_float, "a"}, ?ERROR(["1.2", "a"], [string, float])),
     ?_assertEqual({not_number, "a"}, ?ERROR(["a"], [number])),
     ?_assertEqual(nomatch, ?ERROR(["a"], {first, [int, float]})),
     ?_assertEqual(oops, ?ERROR(["a"], fun (_) -> oops end)),
     ?_assertEqual({unknown_type, my_type}, ?ERROR(["a"], [my_type]))
    ].


param(Key) ->
    erlarg:param(Key, undefined, <<"döc"/utf8>>).

param(Key, Syntax) ->
    erlarg:param(Key, Syntax, <<"döc"/utf8>>).

parse(Args, Spec) ->
    {ok, {Options, []}} = erlarg:parse(Args, Spec),
    Options.

remain(Args, Spec) ->
    {ok, {_, Remaining}} = erlarg:parse(Args, Spec),
    Remaining.

parse_with_error(Args, Spec) ->
    {error, Error} = erlarg:parse(Args, Spec),
    Error.


spec() ->
    spec({any, [limit, filter, delimiter_in, delimiter_out,
                help, version, source]}).

spec(Syntax) ->
    #{
       syntax => Syntax,

       definitions => #{
         limit => param({"-l", "--long"}, int),
         filter => param({"-g", "--grep"},
                         [{any, [invert, columns]}, {search, string}]),
         invert => param({"-v", "--invert-match"}),
         help => param({"-h", "--help"}),
         version => param({"-v", "--version"}),
         columns => param({"-c", "--columns"}, type_columns),
         delimiter_in => param({"-d", "--delimiter"}, type_delimiter),
         delimiter_out => param({undefined, "--out-delimiter"}, type_delimiter),
         stdin => param({"-", "--stdin"}),
         source => param(undefined, [{any, [stdin, {path, string}]}]),

         type_delimiter => fun (["TAB" | Args]) -> {ok, $\t, Args};
                               ([[Char] | Args]) -> {ok, Char, Args};
                               ([Arg | _]) -> {error, {bad_delimiter, Arg}}
                           end,
         type_columns => fun columns/1
        }
      }.

columns([Item | Args]) ->
    case string:split(Item, "*") of
        [_, _] -> {error, {bad_columns, Item}};
        _ -> {ok, Item, Args}
    end.
