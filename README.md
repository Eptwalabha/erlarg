# erlarg

An Erlang lib that parsed a list of arguments into a structured tree.  
Useful for handling options/parameters of escript

> [!WARNING]
> This lib is under development and likely to change in the future (see [TODO](#todo) section).  
> If you need a working alternative, you can use [getopt](https://github.com/jcomellas/getopt).

## install

Add `erlarg` to in your `rebar.config`:
```erlang
{deps, [
 ...
 {erlarg, {git, "https://github.com/Eptwalabha/erlarg.git", {branch, "master"}}}
 ]}.
```
If you're building an `escript`, add `erlarg` to the list of app to include in the binary
```erlang
{escript_incl_apps, [
 ...
 erlarg
 ]}.
```
fetch and compile the dependencies of your project:
```bash
rebar3 compile --deps_only
```
That's it, you're done.

## Usage

```erlang
main(Args) ->
    Spec = {any, [int, string]},
    {ok, Options} = erlarg:parse(Args, Spec),
    ...
```

The `erlarg:parse/2` function takes two parameters:
- `Args` a list of arguments
- `Spec` the spec (or syntax) use to parse `Args` into a structured data that your program can use.

### Spec / Syntax

The spec is a map of two elements:
- `syntax`, it's the syntax tree which can be:
    - base types (`int`, `float`, `string`, `binary`)
    - a custom [parameter](#parameters)
    - a custom type
    - an operator (`and`, `any`, `first`)
    - {name, syntax()}
    - another syntax
- `definitions`: a map of all the parameters and custom type functions used in `syntax`

> [!NOTE]
> If your syntax is relatively simple, you can pass the syntax directly insted of the Spec.

### Syntax

#### base types
The parser will try to "consume" the arguments with the given type.
Currently, there's four basic types available.
- `int`: the parser will try to parse the string (will fail if arg isn't an `int`)
- `float`: same as int, but will succeed even if the arg is an integer
- `string`: does nothing and consume the arg
- `binary`: return the binary representation of the

See the following table
| syntax | arg | result | note |
|---|---|---|---|
| int | "1" | 1 |-|
| int | "1.2" | fails | not an int |
| float | "1.2" | 1.2 |-|
| float | "1" | [1.0] | cast int into float |
| string | "abc" | "abc" | does nothing |
| binary | "abc" | <<"abc">> |-|
| binary | "äbc" | <<"äbc"/utf8>> |-|

#### parameters
TODO

#### operator `and`
format:
```
{and, [syntax()]}
% or more simply
[syntax()]
```
If the syntax is a list, then it means all its child must succeed in order to be succeed.  
The following table use `Args = ["1", "a"]`,
| syntax | result | remaining | note |
|---|---|---|---|
| [int, string] | [1, "a"] | [] |-|
| [int] | [1] | ["a"] |-|
| [string] | ["1"] | ["a"] |-|
| [{opt1, binary}] | [{opt1, <<"1">>}] | ["a"] |-|
| [int, int] | error | ["1", "a"] | "a" isn't an int |
| [int, string, int] | error | ["1", "a"] | missing third arg |
| [{opt1, int}, {opt2, int}] | error | ["1", "a"] | "a" isn't an int |

#### operator `any`
format:
```
{any, [syntax()]}
```
The parser will try to consume arguments as long as one of syntax matches.  
The following table use `Args = ["1", "2", "a", "3", "b"]`
| syntax | result | remaining | note |
|---|---|---|---|
| {any, [int]} | [1, 2] | ["a", "3", "b"] |-|
| {any, [{opt, int}]} | [{opt, 1}, {opt, 2}] | ["a", "3", "b"] |-|
| {any, [int, {b, binary}]} | [1, 2, {b, <<"a">>}, 3, {b, <<"b">>}] | [] |-|
| {any, [string]} | ["1", "2", "a", "3", "b"] | [] |-|

No matter the number of matching syntax, `any` will always succeed. If no element matches no arguments will be consumed.

> [!NOTE]
> Keep in mind that if the list given to `any` contains types like `string` or `binary`, it will consume all the remaining arguments.
> **e.g.** with `{any, [string, custom_type]}`, `custom_type` will never match as `string` will always consume argument

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
