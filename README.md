# erlarg

An Erlang lib that parsed a list of arguments into a structured tree.  
Useful for handling options/parameters of escript

> [!WARNING]
> This lib is under development and is likely to change in the future (see [TODO](#todo) section).  
> If you need a working alternative, you can use [getopt](https://github.com/jcomellas/getopt).


## Installation

Add `erlarg` to in the `deps` of your `rebar.config`:
```erlang
{erlarg, {git, "https://github.com/Eptwalabha/erlarg.git", {branch, "master"}}}
```
If you're building an `escript`, add `erlarg` to the list of apps to include in the binary
```erlang
{escript_incl_apps, [erlarg, …]}.
```
fetch and compile the dependencies of your project:
```bash
rebar3 compile --deps_only
```
That's it, you're done.

## Usage
Let say your escript takes a list of parameters like so:
```bash
./my-script --limit=20 -m 0.25 --format "%s%t" -o output.tsv -
```
The `main/1` function of your script will receive the following list of arguments:
```erlang
["--limit=20", "-m", "0.25", "--format", "%s%t", "-o", "output.tsv", "-"]
```
`erlarg:parse/2` will help you convert it into a structured data:
```erlang
main(Args) ->
    Spec = {any, [{limit, erlarg:param({"-l", "--limit"}, int)},
                  {format, erlarg:param({"-f", "--format"}, binary)},
                  {file, erlarg:param("-o", string)},
                  {stdin, erlarg:param("-")},
                  {max, erlarg:param({"-m", "--max"}, float)}
                  ]}
    {ok, Options} = erlarg:parse(Args, Spec),
    ...
```
In this exemple, `parse` will returns:
```erlang
[{limit, 20},
 {max, 0.25},
 {format, <<"%s%t">>},
 {file, "output.tsv"},
 stdin].
```

This function takes two parameters:
- `Args` the list of arguments
- `Spec` the syntax definition


Spec can either be:
- a map containing two keys: `syntax` and `definitions` (optionnal)
- a `syntax`

The spec is a map of two elements:
- `syntax`, it's the syntax tree which can be:
    - base types (`int`, `float`, `number`, `string`, `binary`, `bool`, `atom`)
    - a [custom parameter](#parameters)
    - a [custom type](#custom-type)
    - an operator (`and`, `any`, `first`)
    - a tuple {key, syntax()}
    - another syntax
- `definitions`: a map of all the parameters and custom type functions used in `syntax`

### Syntax

#### base types
The parser will try to "consume" the arguments with the given types:
- `int`: cast the argument into an int
- `float`: cast the argument into a float (will cast int into float)
- `number`: cast the argument into an int. If it fails it will cast the argument into a float
- `string`: returns the given argument
- `binary`: cast the argument into a binary list
- `atom`: cast the arg to an atom
- `bool`: return the boolean value of the arg

| syntax | arg | result | note |
|---|---|---|---|
| int | "1" | 1 |-|
| int | "1.2" | error | not an int |
| float | "1.2" | 1.2 |-|
| float | "1" | 1.0 | cast int into float |
| float | "1.234e2" | 123.4 |-|
| number | "1" | 1 |-|
| number | "1.2" | 1.2 |-|
| string | "abc" | "abc" | does nothing |
| binary | "äbc" | <<"äbc"/utf8>> | use `unicode:characters_to_binary`|
| atom | "super-top" | 'super-top' |-|

the `bool` conversion:
| arg | bool | note |
|---|---|---|
| "true" | true | case insensitive |
| "yes" | true | case insensitive |
| "yes" | true | any non-empty string |
| "1" | true ||
| "0.00001" | true ||
| "false" | false | case insensitive |
| "no" | false ||
| "" | false | empty-string|
| "0" | false ||
| "0.0" | false ||

> [!NOTE]
> When the parser is task to convert an argument as `string`, `binary`, `bool` or `atom` it will always succeed and consume the argument.
> This detail can be important for some operator like `any`

#### parameters
TODO

#### operator `and`
format:
```
[syntax()]
```
All elements of the list must succeed in order for the operator to succeed.  
The following table use `Args = ["1", "a"]`,
| syntax | args | result | note |
|---|---|---|---|
| [int, string] | ["1", "a"] | [1, "a"] |-|
| [int] | ["1", "a"]  | [1]| remaining: ["a"] |
| [string] | ["1", "a"]  | ["1"] | remaining: ["a"] |-|
| [{key, float}] | ["1.2"] | [{key, 1.2}] |-|
| [int, int] | ["1", "a"] | error | "a" isn't an int |
| [int, string, int] | error | missing third arg |
| [{opt1, int}, {opt2, int}] | ["1", "a"] | error | "a" isn't an int |

#### operator `any`
format:
```
{any, [syntax()]}
```
The parser will try to consume arguments as long as one of syntax matches.  
| syntax | args | result | note |
|---|---|---|---|
| {any, [int]} | ["1", "2", "abc"] | [1, 2] | remaining: ["abc"] |
| {any, [{key, int}]} | ["1", "2"] | [{key, 1}, {key, 2}] |-|
| {any, [int, {s, string}]} | ["1", "2", "abc", "3"] | [1, 2, {s, "abc"}, 3] |-|
| {any, [string]} | ["1", "-o", "abc", "3"] | ["1", "-o", "abc", "3"] | even if "-o" is an option|

No matter the number of matching element, `any` will always succeed. If nothing matches no arguments will be consumed.

> [!NOTE]
> Keep in mind that if the list given to `any` contains types like `string` or `binary`, it will consume all the remaining arguments.  
> `{any, [string, custom_type]}`, `custom_type` will never be executed because the type `string` will always consume argument

#### operator `first`
format:
```
{first, [syntax()]}
```
The parser will return the first syntax to succeed and fails if no syntax matches its list of syntax.  
The following table use `Args = ["a", "b", "1"]`
| syntax | result | remaining | note |
|---|---|---|---|
| {first, [int]} | [1] | ["2", "a", "3", "b"] |-|
| {first, [{opt, int}]} | [{opt, 1}] | ["a", "3", "b"] |-|
| {any, [int, {b, binary}]} | [1, 2, {b, <<"a">>}, 3, {b, <<"b">>}] | [] |-|
| {any, [string]} | ["1", "2", "a", "3", "b"] | [] |-|


#### custom type
Sometime, you need to handle more complexe type.  
In order to do that, give to the parser the function that will handle your type.  
Format:
```
-spec fun((Args :: [string()]) -> {Value :: any(), Args :: [string()]} | Failure :: any()).
```
The function takes a list of arguments to consume.
In case of success, the function must returns a tuple `{any(), args()}`.
Anything else will be considered failure.

In the following exemple, the type `custom_type` is a string with at least two ";" if it matches, it will return the value `{custom, Reverse}` where `Reverse` is the consumed `Arg` reversed.
```erlang
main(Arg) ->
    Spec = #{ syntax => {any, [my_type]},
              types => #{
                my_type => fun custom_type/1
              }
            },
    {ok, Options}

custom_type([Arg | Args]) ->
    case string:split(Arg, ";", all) of
        [_, _, _ | _] ->
            {{custom, lists:reverse(Arg)}, Args};
        _ -> fails
    end.
```
Exemples:  
The results of the following table use the type `my_type` defined by the following function:
```
my_type(["c:" ++ Arg | Args]) ->
    {{cust, Arg}, Args};
my_type(_) ->
    fails.
```
| syntax | args | result | remaining | note |
|---|---|---|---|---|
| [my_type] | ["c:abc"] | [{cust, "abc"}] | [] |-|
| {any, [my_type, int]} | ["1", "c:abc", "other"] | [1, {cust, "abc"}] | ["other"] |-|
| [my_type, my_type] | ["c:abc", "other] | error | ["c:abc", "other] | the second arg does not match type `my_type` |
| [{opt1, binary}] | [{opt1, <<"1">>}] | ["a"] |-|
| [int, int] | error | ["1", "a"] | "a" isn't an int |
| [int, string, int] | error | ["1", "a"] | missing third arg |
| [{opt1, int}, {opt2, int}] | error | ["1", "a"] | "a" isn't an int |

## TODO

> [!NOTE]
> This lib will enter v1.0.0 once the remaining points bellow are dealt with

- [ ] complete README.md
- [ ] handle remaining arguments ?
- [ ] prevent infinite / cyclic recursion
- [ ] add more tests

additional features (plan for next version):
- [ ] `erlarg:usage/1`
- [ ] `--` (see: https://unix.stackexchange.com/a/11382)
- [ ] optimize the spec
- [ ] operator `once`
- [ ] operator `not`