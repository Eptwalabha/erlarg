-module(erlarg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ERROR(Args, Specs), parse(Args, Specs, error)).
-define(RESULT(Args, Specs), parse(Args, Specs, result)).
-define(REMAIN(Args, Specs), parse(Args, Specs, remain)).

parse_syntax_directly_test_() ->
    [?_assertEqual(["a", "b", "c"], ?RESULT(["a", "b", "c"], {any, [string]})),
     ?_assertEqual([1, 2, 3], ?RESULT(["1", "2", "3"], {any, [int]}))
    ].

parse_basic_types_test_() ->
    [?_assertEqual([123], ?RESULT(["123"], int)),
     ?_assertEqual([123.4], ?RESULT(["123.4"], float)),
     ?_assertEqual([123.0], ?RESULT(["123"], float)),
     ?_assertEqual([123.4], ?RESULT(["1.234e2"], float)),
     ?_assertEqual([123], ?RESULT(["123"], number)),
     ?_assertEqual([123.4], ?RESULT(["123.4"], number)),
     ?_assertEqual([123.4], ?RESULT(["1.234e2"], number)),
     ?_assertEqual([-123.4], ?RESULT(["-1.234e2"], number)),
     ?_assertEqual(["123"], ?RESULT(["123"], string)),
     ?_assertEqual([<<"äbc"/utf8>>], ?RESULT(["äbc"], binary)),
     ?_assertEqual(['äbc'], ?RESULT(["äbc"], atom)),
     ?_assertEqual([{key, 123}], ?RESULT(["123"], {key, int})),
     ?_assertEqual([{key, 123}], ?RESULT(["123"], {key, number})),
     ?_assertEqual([{key, 123.4}], ?RESULT(["123.4"], {key, float})),
     ?_assertEqual([{key, "123"}], ?RESULT(["123"], {key, string})),
     ?_assertEqual([{key, <<"äbc"/utf8>>}], ?RESULT(["äbc"], {key, binary})),
     ?_assertEqual([{key, 'äbc'}], ?RESULT(["äbc"], {key, atom}))
    ].

parse_boolean_test_() ->
    False = ["false", "f", "0", "0.0000", "no", "n",
             "False", "fAlSe", "disabled", ""],
    True = ["true", "t", "1", "1000", "0.00001", "yes", "y",
            "True", "tRuE", "enabled", "abcd"],
    [[?_assertEqual([false], ?RESULT([Arg], bool)) || Arg <- False],
     [?_assertEqual([true], ?RESULT([Arg], bool)) || Arg <- True]].

parse_named_type_test_() ->
    [?_assertEqual([{tag, "abc"}], ?RESULT(["abc"], {tag, string})),
     ?_assertEqual([], ?REMAIN(["abc"], {tag, string})),

     ?_assertEqual([{tag, 123}], ?RESULT(["123"], {tag, int})),
     ?_assertEqual([], ?REMAIN(["abc"], {tag, string})),

     ?_assertEqual([{a, [1, "2"]}], ?RESULT(["1", "2"], {a, [int, string]})),
     ?_assertEqual([{a, ["abc", {a2, 2.3}]}, {b, <<"bin">>}],
                   ?RESULT(["abc", "2.3", "bin"],
                           [{a, [string, {a2, float}]}, {b, binary}]))
    ].

parse_opt_test_() ->
    [?_assertEqual([{option, 123}],
                   ?RESULT(["-o", "123"], opt("-o", option, int))),
     ?_assertEqual([{option, "123"}],
                   ?RESULT(["--option", "123"],
                           opt({"-o", "--option"}, option, string))),
     ?_assertEqual([{option, 123.0}],
                   ?RESULT(["--option=123.0"],
                           opt({"-o", "--option"}, option, float))),
     ?_assertEqual([option],
                   ?RESULT(["-o"], opt({"-o", "--option"}, option))),
     ?_assertEqual([{option, <<"äbc"/utf8>>}],
                   ?RESULT(["äbc"], opt(undefined, option, binary)))
    ].

parse_any_test_() ->
    [?_assertEqual(["-a", "2"], ?RESULT(["-a", "2"], {any, [string]})),
     ?_assertEqual(["-a", "2"], ?RESULT(["-a", "2"], {any, [string, int]})),
     ?_assertEqual(["a", 2, "c", 4],
                   ?RESULT(["a", "2", "c", "4"], {any, [int, string]})),
     ?_assertEqual(["a", {i, 2}, "c", {i, 4}],
                   ?RESULT(["a", "2", "c", "4"],
                        {any, [{i, int}, string, {n, int}]})),
     ?_assertEqual(["-a", 2],
                   ?RESULT(["-a", "2"],
                        [string, int])),
     ?_assertEqual(["-a", {a, 2}, {b, "-c"}, 4],
                   ?RESULT(["-a", "2", "-c", "4"],
                        [string, {a, int}, {b, string}, int])),
     ?_assertEqual([1, 2, "a", "1"],
                   ?RESULT(["1", "2", "a", "1"],
                         [{any, [int]}, {any, [string]}])),
     ?_assertEqual([1, {a, 2}, {b, [3]}],
                   ?RESULT(["1", "2", "3"],
                        [int, {a, int}, {b, [int]}])),
     ?_assertEqual([<<"1">>, {a, <<"2">>}, {b, [<<"3">>]}],
                   ?RESULT(["1", "2", "3"],
                        [binary, {a, binary}, {b, [binary]}]))
    ].


parse_first_test_() ->
    [?_assertEqual([1], ?RESULT(["1"], {first, [int, string]})),
     ?_assertEqual(["abc"], ?RESULT(["abc"], {first, [int, string]})),
     ?_assertEqual([{f, 1.1}],
                   ?RESULT(["1.1"], {first, [{i, int}, {f, float}]})),
     ?_assertEqual([{d, [1, "abc"]}],
                   ?RESULT(["1", "abc"],
                         {first, [{a, [int, int]},
                                  {b, [float, float]},
                                  {c, [int, string, int]},
                                  {d, [number, string]}]})),
     ?_assertEqual([], ?RESULT(["a"], {first, [int, float]}))
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
    [?_assertEqual([{0, "cba"}], ?RESULT(["abc"], CustomFun)),
     ?_assertEqual([{0, "cba"}], ?RESULT(["abc"], Spec(custom))),
     ?_assertEqual([{2, "ba"}, "boom", {0, "dc"}],
                   ?RESULT(["ab", "boom", "cd"], Spec({any, [custom, string]})))
    ].

multi_params_test_() ->
    Spec = #{ syntax => [a, b],
              definitions => #{
                a => opt("-a", a, [string, int]),
                b => opt("-b", b, [{item1, string}, {item2, string}])
               }
            },
    [?_assertEqual([{a, ["abc", 1]}, {b, [{item1, "def"}, {item2, "2"}]}],
                   ?RESULT(["-a", "abc", "1", "-b", "def", "2"], Spec))
    ].


parse_big_test_() ->
    [?_assertEqual([version], ?RESULT(["-v"], spec())),
     ?_assertEqual([help, help, {source, [stdin, {path, "toto.tsv"}]}],
                   ?RESULT(["-h", "--help", "-", "toto.tsv"], spec())),
     ?_assertEqual([{filter, [invert, {columns, "a,b"}, {search, "search A"}]},
                    {filter, [invert, {search, "search B"}]},
                    {filter, [{columns, "a,b"}, {search, "search C"}]},
                    {filter, [{search, "search D"}]},
                    {source, [{path, "toto"}]}
                   ],
                   ?RESULT(["--grep", "--invert-match", "-c", "a,b", "search A",
                            "-g", "-v", "search B",
                            "-g", "--columns", "a,b", "search C",
                            "--grep=search D", "toto"], spec()))
    ].

parse_option_test_() ->
    Spec = #{ definitions => #{
                a => opt({"-a", "--a-long"}, a, {first, [int, string]}),
                b => opt("-b", b, [{any, [a, [{b, int}, {bs, string}]]}, c]),
                c => opt(undefined, c, int)
               }
            },
    Test = fun (Args, Syntax) ->
                   ?RESULT(Args, Spec#{ syntax => Syntax })
           end,
    [?_assertEqual([{a, 1}, {b, [{b, 2}, {bs, "3"}, {c, 4}]}],
                   Test(["--a-long=1", "-b", "2", "3", "4"], [a, b]))
    ].


parse_match_option_test_() ->
    OptionA = opt({"-a", "--option-a"}, option_a),
    OptionB = opt({"-b", "--option-b"}, option_b),
    OptionC = opt({"-c", "--option-c"}, option_c, string),
    [?_assertEqual([option_a], ?RESULT(["-a"], OptionA)),
     ?_assertEqual([option_a], ?RESULT(["--option-a"], OptionA)),
     ?_assertEqual([option_a, option_b],
                   ?RESULT(["-a", "-b"], [OptionA, OptionB])),
     {"sort option combination is allowed",
      ?_assertEqual([option_a, option_b],
                    ?RESULT(["-ab"], [OptionA, OptionB]))},
     {"short option can repeat",
      ?_assertEqual([option_a, option_b, option_b, option_a],
                    ?RESULT(["-abba"], {any, [OptionA, OptionB]}))},
     {"short option combination can end with an option with parameter",
      ?_assertEqual([option_a, option_b, {option_c, "abc"}],
                    ?RESULT(["-abc", "abc"], [OptionA, OptionB, OptionC]))},
     ?_assertEqual([{option_c, "abc"}],
                   ?RESULT(["-c", "abc"], [OptionC])),
     ?_assertEqual([{option_c, "abc"}],
                   ?RESULT(["--option-c=abc"], [OptionC])),
     ?_assertEqual([{option_c, "abc"}],
                   ?RESULT(["--option-c", "abc"], [OptionC])),
     {"format shortVALUE is allowed",
      ?_assertEqual([{option_c, "abc"}],
                    ?RESULT(["-cabc"], [OptionC]))},
     {"short option combined with shortVALUE is allowed",
      ?_assertEqual([option_a, option_b, {option_c, "b"}],
                    ?RESULT(["-abcb"], [OptionA, OptionB, OptionC]))},

     {"longVALUE format is not allowed",
      ?_assertEqual({not_opt, OptionC, "--option-cabc"},
                    ?ERROR(["--option-cabc"], [OptionC]))}
    ].


parse_readme_example_test() ->
    Spec = {any, [opt({"-l", "--limit"}, limit, int),
                  opt({"-f", "--format"}, format, binary),
                  opt("-o", file, string),
                  opt("-", stdin),
                  opt({"-m", "--max"}, max, float)]},
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
    [?_assertEqual(["b", "c"], ?REMAIN(["a", "b", "c"], string)),
     ?_assertEqual(["c"], ?REMAIN(["1", "b", "c"], [int, string])),
     ?_assertEqual([], ?REMAIN(["a", "b", "c"], {any, string})),
     ?_assertEqual(["2.2", "c"], ?REMAIN(["1", "2.2", "c"], {any, int})),
     ?_assertEqual(["c"], ?REMAIN(["1", "2.2", "c"], {any, float}))
    ].


error_aliases_test_() ->
    [?_assertEqual({unknown_alias, my_type}, ?ERROR(["a"], [my_type])),
     ?_assertEqual({unknown_alias, my_type}, ?ERROR(["a"], [{key, my_type}]))
    ].


error_option_badarg_test_() ->
    SubOption = opt({undefined, "--sub"}, subopt, [string, int]),
    Option = opt({undefined, "--opt"}, opt, {any, [int, SubOption]}),
    [{"throws an error if option's missing an argument",
      ?_assertEqual({bad_opt, opt("-a", name, int), {missing, int}},
                    ?ERROR(["-a"], opt("-a", name, int)))},
     {"option fails to cast string into int",
      ?_assertEqual({bad_opt, opt("-a", name, int), {not_int, "abc"}},
                    ?ERROR(["-a", "abc"],
                           [opt("-a", name, int)]))},

     {"option b matches but fails to cast the argument into float",
      ?_assertEqual({bad_opt, opt("-b", b, {key, float}), {not_float, "abc"}},
                    ?ERROR(["-b", "abc"],
                           {any, [opt("-a", a),
                                  opt("-b", b, {key, float})]}))},

     [{"'" ++ BadArg ++ "' shouldn't match the option '-a'",
       ?_assertEqual({not_opt, opt("-a"), BadArg},
                     ?ERROR([BadArg], [opt("-a")]))}
      || BadArg <- ["-b", "--a", "--not-a"]],

     {"option fails if sub-option fails",
      ?_assertEqual({bad_opt, Option,
                     {bad_opt, SubOption, {missing, [int]}}},
                    ?ERROR(["--opt", "1", "--sub", "2"], [Option]))}
    ].

error_test_() ->
    [?_assertEqual({not_int, "a"}, ?ERROR(["1", "a"], [int, int])),
     ?_assertEqual({not_int, "1.2"}, ?ERROR(["a", "1.2"], [string, int])),
     ?_assertEqual({not_float, "a"}, ?ERROR(["1.2", "a"], [string, float])),
     ?_assertEqual({not_number, "a"}, ?ERROR(["a"], [number])),
     ?_assertEqual(oops, ?ERROR(["a"], fun (_) -> oops end)),
     ?_assertEqual({bad_syntax, undefined}, ?ERROR(["abc"], {tag, undefined}))
    ].


opt(Key) ->
    erlarg:opt(Key, opt_name).

opt(Key, Name) ->
    erlarg:opt(Key, Name).

opt(Key, Name, Syntax) ->
    erlarg:opt(Key, Name, Syntax).

parse(Args, Spec, Type) ->
    case erlarg:parse(Args, Spec) of
        {ok, {Options, _}} when Type =:= result -> Options;
        {ok, {_, RemainArgs}} when Type =:= remain -> RemainArgs;
        {error, Error} when Type =:= error -> Error;
        _ -> failure
    end.


spec() ->
    spec({any, [limit, filter, delimiter_in, delimiter_out,
                help, version, source]}).

spec(Syntax) ->
    #{
       syntax => Syntax,

       definitions => #{
         limit => opt({"-l", "--long"}, limit, int),
         filter => opt({"-g", "--grep"}, filter,
                         [{any, [invert, columns]}, {search, string}]),
         invert => opt({"-v", "--invert-match"}, invert),
         help => opt({"-h", "--help"}, help),
         version => opt({"-v", "--version"}, version),
         columns => opt({"-c", "--columns"}, columns, type_columns),
         delimiter_in => opt({"-d", "--delimiter"},
                             delimiter_in, type_delimiter),
         delimiter_out => opt({undefined, "--out-delimiter"},
                              delimiter_out, type_delimiter),
         stdin => opt({"-", "--stdin"}, stdin),
         source => opt(undefined, source, [{any, [stdin, {path, string}]}]),

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
