-module(erlarg_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_RESULT(Expected, Args, Syntax),
        ?_assertEqual(Expected, parse(Args, Syntax, result))).
-define(TEST_RESULT(Expected, Args, Syntax, Aliases),
        ?_assertEqual(Expected, parse(Args, Syntax, Aliases, result))).
-define(TEST_REMAIN(Expected, Args, Syntax),
        ?_assertEqual(Expected, parse(Args, Syntax, remain))).
-define(TEST_ERROR(Expected, Args, Syntax),
        ?_assertEqual(Expected, parse(Args, Syntax, error))).
-define(TEST_ERROR(Expected, Args, Syntax, Aliases),
        ?_assertEqual(Expected, parse(Args, Syntax, Aliases, error))).

parse_syntax_directly_test_() ->
    [?TEST_RESULT(["a", "b", "c"],  ["a", "b", "c"],    {any, [string]}),
     ?TEST_RESULT([1, 2, 3],        ["1", "2", "3"],    {any, [int]})
    ].

parse_basic_types_test_() ->
    [?TEST_RESULT([123],            ["123"],        int),
     ?TEST_RESULT([123.4],          ["123.4"],      float),
     ?TEST_RESULT([123.0],          ["123"],        float),
     ?TEST_RESULT([123.4],          ["1.234e2"],    float),
     ?TEST_RESULT([123],            ["123"],        number),
     ?TEST_RESULT([123.4],          ["123.4"],      number),
     ?TEST_RESULT([123.4],          ["1.234e2"],    number),
     ?TEST_RESULT([-12.3],          ["-1.23e1"],    number),
     ?TEST_RESULT(["123"],          ["123"],        string),
     ?TEST_RESULT(['äbc'],          ["äbc"],        atom),
     ?TEST_RESULT([<<"äbc"/utf8>>], ["äbc"],        binary)
    ].

parse_named_value_test_() ->
    [?TEST_RESULT([{key, 123}],             ["123"],        {key, int}),
     ?TEST_RESULT([{key, 123}],             ["123"],        {key, number}),
     ?TEST_RESULT([{key, 123.4}],           ["123.4"],      {key, float}),
     ?TEST_RESULT([{key, "123"}],           ["123"],        {key, string}),
     ?TEST_RESULT([{key, 'äbc'}],           ["äbc"],        {key, atom}),
     ?TEST_RESULT([{key, <<"äbc"/utf8>>}],  ["äbc"],        {key, binary})
    ].

parse_truthy_boolean_test_() ->
    True = ["true",
            "t",
            "1",
            "1000",
            "0.00001",
            "yes",
            "y",
            "True",
            "tRuE",
            "enabled",
            "abcd"],
    [{"'" ++ Arg ++ "' is truthy",
      ?TEST_RESULT([true], [Arg], bool)} || Arg <- True].

parse_falsy_boolean_test_() ->
    False = ["false", "f", "0", "0.0000", "no", "n",
             "False", "fAlSe", "disabled", ""],
    [{"'" ++ Arg ++ "' is falsy",
      ?TEST_RESULT([false], [Arg], bool)} || Arg <- False].

parse_named_type_test_() ->
    [?TEST_RESULT([{tag, "abc"}],   ["abc"],    {tag, string}),
     ?TEST_REMAIN([],               ["abc"],    {tag, string}),

     ?TEST_RESULT([{tag, 123}],     ["123"],    {tag, int}),
     ?TEST_REMAIN([],               ["123"],    {tag, int}),

     ?TEST_RESULT([{a, [1, "2"]}], ["1", "2"], {a, [int, string]}),
     ?TEST_RESULT([{a, ["abc", {a2, 2.3}]}, {b, <<"bin">>}],
                  ["abc", "2.3", "bin"],
                  [{a, [string, {a2, float}]}, {b, binary}])
    ].

parse_opt_test_() ->
    [?TEST_RESULT([{option, 123}],
                  ["-o", "123"],
                  opt("-o", option, int)),
     ?TEST_RESULT([{option, "123"}],
                  ["--option", "123"],
                  opt({"-o", "--option"}, option, string)),
     ?TEST_RESULT([{option, 123.0}],
                  ["--option=123.0"],
                  opt({"-o", "--option"}, option, float)),
     ?TEST_RESULT([option],
                  ["-o"],
                  opt({"-o", "--option"}, option)),
     ?TEST_RESULT([{option, <<"äbc"/utf8>>}],
                  ["äbc"],
                  opt(undefined, option, binary))
    ].

parse_any_test_() ->
    [?TEST_RESULT(["-a", "2"],  ["-a", "2"],    {any, [string]}),
     ?TEST_RESULT(["-a", "2"],  ["-a", "2"],    {any, [string, int]}),
     ?TEST_RESULT(["a", 2, "c", 4],
                  ["a", "2", "c", "4"],
                  {any, [int, string]}),
     ?TEST_RESULT(["a", {i, 2}, "c", {i, 4}],
                  ["a", "2", "c", "4"],
                  {any, [{i, int}, string, {n, int}]}),
     ?TEST_RESULT(["-a", 2],    ["-a", "2"],    [string, int]),
     ?TEST_RESULT(["-a", {a, 2}, {b, "-c"}, 4],
                  ["-a", "2", "-c", "4"],
                  [string, {a, int}, {b, string}, int]),
     ?TEST_RESULT([1, 2, "a", "1"],
                  ["1", "2", "a", "1"],
                  [{any, [int]}, {any, [string]}]),
     ?TEST_RESULT([1, {a, 2}, {b, [3]}],
                  ["1", "2", "3"],
                  [int, {a, int}, {b, [int]}]),
     ?TEST_RESULT([<<"1">>, {a, <<"2">>}, {b, [<<"3">>]}],
                  ["1", "2", "3"],
                  [binary, {a, binary}, {b, [binary]}])
    ].


prevent_infinit_loop_test_() ->
    Aliases = #{
      a => {infinity, b},
      b => {infinity, a}
     },
    {spawn,
     {timeout, 0.5, ?TEST_ERROR(max_recursion, ["abc"], a, Aliases)}}.


parse_first_test_() ->
    [?TEST_RESULT([1],          ["1"],      {first, [int, string]}),
     ?TEST_RESULT(["abc"],      ["abc"],    {first, [int, string]}),
     ?TEST_RESULT([{f, 1.1}],   ["1.1"],    {first, [{i, int}, {f, float}]}),
     ?TEST_RESULT([],           ["a"],      {first, [int, float]}),
     ?TEST_RESULT([{d, [1, "abc"]}],
                  ["1", "abc"],
                  {first, [{a, [int, int]},
                           {b, [float, float]},
                           {c, [int, string, int]},
                           {d, [number, string]}]})
    ].

parse_custom_types_test_() ->
    CustomFun = fun (["boom" | _]) ->
                        fails;
                    ([Arg | Args]) ->
                        {ok, {length(Args), lists:reverse(Arg)}, Args}
                end,
    Aliases = #{ custom => CustomFun },
    [?TEST_RESULT([{0, "cba"}], ["abc"], CustomFun),
     ?TEST_RESULT([{0, "cba"}], ["abc"], custom, Aliases),
     ?TEST_RESULT([{2, "ba"}, "boom", {0, "dc"}],
                   ["ab", "boom", "cd"],
                   {any, [custom, string]}, Aliases)
    ].


multi_params_test_() ->
    Aliases = #{
      a => opt("-a", a, [string, int]),
      b => opt("-b", b, [{item1, string}, {item2, string}])
     },
    [?TEST_RESULT([{a, ["abc", 1]}, {b, [{item1, "def"}, {item2, "2"}]}],
                  ["-a", "abc", "1", "-b", "def", "2"],
                  [a, b], Aliases)
    ].


parse_big_test_() ->
    Syntax = {any, [limit, filter, delimiter_in, delimiter_out,
                    help, version, source]},
    Aliases = #{
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
     },
    [?TEST_RESULT([version], ["-v"], Syntax, Aliases),
     ?TEST_RESULT([help, help, {source, [stdin, {path, "toto.tsv"}]}],
                  ["-h", "--help", "-", "toto.tsv"],
                  Syntax, Aliases),
     ?TEST_RESULT([{filter, [invert, {columns, "a,b"}, {search, "search A"}]},
                   {filter, [invert, {search, "search B"}]},
                   {filter, [{columns, "a,b"}, {search, "search C"}]},
                   {filter, [{search, "search D"}]},
                   {source, [{path, "toto"}]}],
                  ["--grep", "--invert-match", "-c", "a,b", "search A",
                   "-g", "-v", "search B",
                   "-g", "--columns", "a,b", "search C",
                   "--grep=search D", "toto"],
                  Syntax, Aliases)
    ].

parse_option_test_() ->
    Aliases = #{
      a => opt({"-a", "--a-long"}, a, {first, [int, string]}),
      b => opt("-b", b, [{any, [a, [{b, int}, {bs, string}]]}, c]),
      c => opt(undefined, c, int)
     },
    ?TEST_RESULT([{a, 1}, {b, [{b, 2}, {bs, "3"}, {c, 4}]}],
                 ["--a-long=1", "-b", "2", "3", "4"],
                 [a, b], Aliases).


parse_match_option_test_() ->
    OptionA = opt({"-a", "--option-a"}, option_a),
    OptionB = opt({"-b", "--option-b"}, option_b),
    OptionC = opt({"-c", "--option-c"}, option_c, string),
    [?TEST_RESULT([option_a],           ["-a"],         OptionA),
     ?TEST_RESULT([option_a],           ["--option-a"], OptionA),
     ?TEST_RESULT([option_a, option_b],
                  ["-a", "-b"],
                  [OptionA, OptionB]),
     {"sort option combination is allowed",
      ?TEST_RESULT([option_a, option_b],
                   ["-ab"],
                   [OptionA, OptionB])},
     {"short option can repeat",
      ?TEST_RESULT([option_a, option_b, option_b, option_a],
                   ["-abba"],
                   {any, [OptionA, OptionB]})},
     {"short option combination can end with an option with parameter",
      ?TEST_RESULT([option_a, option_b, {option_c, "abc"}],
                   ["-abc", "abc"],
                   [OptionA, OptionB, OptionC])},
     {"short VALUE format",
      ?TEST_RESULT([{option_c, "abc"}], ["-c", "abc"],          [OptionC])},
     {"long VALUE format",
      ?TEST_RESULT([{option_c, "abc"}], ["--option-c", "abc"],  [OptionC])},
     {"long=VALUE format",
      ?TEST_RESULT([{option_c, "abc"}], ["--option-c=abc"],     [OptionC])},
     {"format shortVALUE is allowed",
      ?TEST_RESULT([{option_c, "abc"}], ["-cabc"],              [OptionC])},
     {"short option combined with shortVALUE is allowed",
      ?TEST_RESULT([option_a, option_b, {option_c, "b"}],
                   ["-abcb"],
                   [OptionA, OptionB, OptionC])},

     {"longVALUE format is not allowed",
      ?TEST_ERROR({not_opt, OptionC, "--option-cabc"},
                  ["--option-cabc"],
                  [OptionC])}
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
    [?TEST_REMAIN(["b", "c"],   ["a", "b", "c"],    string),
     ?TEST_REMAIN(["c"],        ["1", "b", "c"],    [int, string]),
     ?TEST_REMAIN([],           ["a", "b", "c"],    {any, string}),
     ?TEST_REMAIN(["2.2", "c"], ["1", "2.2", "c"],  {any, int}),
     ?TEST_REMAIN(["c"],        ["1", "2.2", "c"],  {any, float})
    ].


error_aliases_test_() ->
    [?TEST_ERROR({unknown_alias, my_type}, ["a"], [my_type]),
     ?TEST_ERROR({unknown_alias, my_type}, ["a"], [{key, my_type}])
    ].


error_option_badarg_test_() ->
    SubOption = opt({undefined, "--sub"}, subopt, [string, int]),
    Option = opt({undefined, "--opt"}, opt, {any, [int, SubOption]}),
    [{"throws an error if option's missing an argument",
      ?TEST_ERROR({bad_opt, opt("-a", name, int), {missing, int}},
                  ["-a"],
                  opt("-a", name, int))},
     {"option fails to cast string into int",
      ?TEST_ERROR({bad_opt, opt("-a", name, int), {not_int, "abc"}},
                  ["-a", "abc"],
                  [opt("-a", name, int)])},

     {"option b matches but fails to cast the argument into float",
      ?TEST_ERROR({bad_opt, opt("-b", b, {key, float}), {not_float, "abc"}},
                  ["-b", "abc"],
                  {any, [opt("-a", a),
                         opt("-b", b, {key, float})]})},

     [{"'" ++ BadArg ++ "' shouldn't match the option '-a'",
       ?TEST_ERROR({not_opt, opt("-a"), BadArg}, [BadArg], [opt("-a")])}
      || BadArg <- ["-b", "--a", "--not-a"]],

     {"option fails if sub-option fails",
      ?TEST_ERROR({bad_opt, Option,
                   {bad_opt, SubOption, {missing, [int]}}},
                  ["--opt", "1", "--sub", "2"],
                  [Option])}
    ].

error_test_() ->
    [?TEST_ERROR({not_int, "a"},    ["1", "a"],     [int, int]),
     ?TEST_ERROR({not_int, "1.2"},  ["a", "1.2"],   [string, int]),
     ?TEST_ERROR({not_float, "a"},  ["1.2", "a"],   [string, float]),
     ?TEST_ERROR({not_number, "a"}, ["a"],          [number]),
     ?TEST_ERROR(oops,              ["a"],          fun (_) -> oops end),
     ?TEST_ERROR({bad_syntax, undefined},  ["abc"], {tag, undefined}),
     ?TEST_ERROR({unknown_alias, unknown}, ["abc"], {tag, unknown})
    ].


opt(Key) ->
    erlarg:opt(Key, opt_name).

opt(Key, Name) ->
    erlarg:opt(Key, Name).

opt(Key, Name, Syntax) ->
    erlarg:opt(Key, Name, Syntax).

parse(Args, Syntax, Type) ->
    case erlarg:parse(Args, Syntax) of
        {ok, {Options, _}} when Type =:= result -> Options;
        {ok, {_, RemainArgs}} when Type =:= remain -> RemainArgs;
        {error, Error} when Type =:= error -> Error;
        _ -> failure
    end.

parse(Args, Syntax, Aliases, Type) ->
    case erlarg:parse(Args, Syntax, Aliases) of
        {ok, {Options, _}} when Type =:= result -> Options;
        {ok, {_, RemainArgs}} when Type =:= remain -> RemainArgs;
        {error, Error} when Type =:= error -> Error;
        _ -> failure
    end.


columns([Item | Args]) ->
    case string:split(Item, "*") of
        [_, _] -> {error, {bad_columns, Item}};
        _ -> {ok, Item, Args}
    end.
